import System.Process
import System.IO (hGetLine, hIsEOF, hPutStrLn, hFlush, Handle)
import System.Environment (getExecutablePath)
import System.Directory (doesFileExist)
import Control.Concurrent.Thread.Delay
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans

import Data.Int
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Aeson as Aeson
import Data.Aeson.Types
import Data.List

import qualified Settings
import IPC
import Bot
import Connection

type ModuleProcess = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
data KernelState = KernelState {
	getKernelModules :: [(ModuleProcess, String)],
	getKernelSubscribers :: [String],
	getKernelAdmin :: String,
	getKernelResolvingCommands :: [(String, [String])] -- (command, voters)
}

main = do
	token <- Settings.readToken
	runBot token cannotConnect kernel

cannotConnect = do
	putStrLn "cannot retrieve a websocket url. exits..."

kernel bot conn = do
	putStrLn "Connected to clug slack"
	putStrLn $ "bot name: " ++ (getSelfName $ getSelf bot)
	putStrLn $ "bot id:   " ++ (getSelfID $ getSelf bot)
	kstate <- newMVar $ KernelState [] ["codeonwort"] "codeonwort" []
	--sendMsg conn (Message 1 "C04TJBLCG" "kernel loaded")
	let loop = do
		recvMsg conn >>= handleInput bot conn kstate
		delay 33333 >> loop --30fps
	let loop2 = do
		line <- T.getLine
		unless (T.null line) $ sendRaw conn line >> loop2
	forkIO loop
	loop

handleInput bot conn kstate jsonMsg = case jsonMsg of
	Just json -> do
		let msgType = getPropParser json "type" :: Result String
		case msgType of
			Success "message" ->
				if hasProp json "subtype"
				then return ()
				else handleInput_message bot conn kstate json
			_ -> putStrLn "=> unrecognized message"
	Nothing -> do
		putStrLn "slack sent me non-json data... wtf?"
		putStrLn $ "=> " ++ (show jsonMsg)

startVote bot kstate conn json = do
	let msg = parseMessage json
	let chan = receiveMessage_channel msg
	let userID = receiveMessage_user msg
	let cmd = receiveMessage_text msg
	let user = getUserName $ getUserWithID bot userID
	--let botName = (getSelfName . getSelf) bot
	--let botID = (getSelfID . getSelf) bot
	ks <- takeMVar kstate
	let subscribers = getKernelSubscribers ks
	let cmd_list_old = getKernelResolvingCommands ks
	if user `notElem` subscribers
	then do
		sendMsg conn $ Message 12 chan (user ++ "님은 투표권이 없습니다.")
		putMVar kstate ks
	else do
		(idx,first) <- case findIndex (\(resolving,_) -> cmd == resolving) cmd_list_old of
			Nothing -> do
				sendMsg conn $ Message 1 chan (user ++ "님이 다음 안건을 의결했습니다: `" ++ cmd ++ "`\n찬성하시면 동일 안건을 30초 내에 입력해주세요.")
				putMVar kstate $ ks { getKernelResolvingCommands = (cmd,[user]):cmd_list_old }
				return (0,True)
			Just idx' -> putMVar kstate ks >> return (idx',False)
		ks <- takeMVar kstate
		let cmd_list = getKernelResolvingCommands ks
		let (_, voters) = cmd_list !! idx
		if (user `elem` voters && first == False)
		then do
			sendMsg conn $ Message 1 chan (user ++ "님은 이미 이 안건에 찬성했습니다")
			putMVar kstate ks
		else do
			when (first == False) $ sendMsg conn $ Message 10 chan (user ++ "님이 찬성했습니다")
			let cmd_list' = (cmd, user:voters) : (deleteAt cmd_list idx)
			putMVar kstate $ ks { getKernelResolvingCommands = cmd_list' }
		ks <- takeMVar kstate
		let majority = ((length . getKernelSubscribers) ks) `div` 2
		if (length voters) > majority
		then do
			sendMsg conn $ Message 1 chan ("`" ++ cmd ++ "` 찬성이 과반수를 넘어 집행을 시작합니다.")
			putMVar kstate $ ks { getKernelResolvingCommands = deleteAt cmd_list idx }
			executeResolution bot kstate conn json
		else putMVar kstate ks
		
deleteAt ls idx = take idx ls ++ drop (idx + 1) ls

-- kstate :: MVar KernelState
handleInput_message bot conn kstate json = do
	let msg = parseMessage json
	let cmd = receiveMessage_text msg
	let botName = (getSelfName . getSelf) bot
	if botName `isPrefixOf` cmd
	then startVote bot kstate conn json
	else return ()

executeResolution bot kstate conn json = do
	let msg = parseMessage json
	let chan = receiveMessage_channel msg
	let usr = receiveMessage_user msg
	let txt = receiveMessage_text msg
	let usrName = getUserName $ getUserWithID bot usr
	let botName = (getSelfName . getSelf) bot
	let botID = (getSelfID . getSelf) bot
	when (txt == botName ++ " status") $ do
		ks <- takeMVar kstate
		let mod_list = getKernelModules ks
		sendMsg conn $ Message 2 chan ("my name is " ++ botName ++ "\nmy id is " ++ botID)
		sendMsg conn $ Message 3 chan ("admin: " ++ (getKernelAdmin ks))
		sendMsg conn $ Message 4 chan ("subscribers: " ++ (show $ getKernelSubscribers ks))
		sendMsg conn $ Message 5 chan ("loaded modules: " ++ (show $ map (\(_,n)->n) $ getKernelModules ks))
		putMVar kstate ks
	when (txt == botName ++ " help") $ do
		sendMsg conn $ Message 3 chan ("나는 나보다 약한 녀석의 명령 따윈 듣지 않는다.")
	when (txt == botName ++ " 맞아 아니야") $ do
		sendMsg conn $ Message 4 chan ("내가 어떻게 알아")
	--
	let prefix_load_module = botName ++ " load-module "
	when (prefix_load_module `isPrefixOf` txt) $ do
		let module_name = (length prefix_load_module) `drop` txt
		module_path <- getModulePath module_name
		can_load <- doesFileExist module_path
		if can_load
		then do
			found <- moduleRunning kstate module_name
			case found of
				Just _ -> sendMsg conn $ Message 5 chan (module_name ++ "은 이미 로드되었습니다")
				Nothing -> do
					modProc <- createProcess $ (proc module_path []){ std_in = CreatePipe, std_out = CreatePipe }
					ks <- takeMVar kstate
					let mod_list = getKernelModules ks
					putMVar kstate $ ks { getKernelModules = (modProc,module_name):mod_list }
					tid <- forkIO $ runModule bot conn modProc module_name chan
					sendMsg conn $ Message 5 chan (module_name ++ " 모듈을 실행합니다")
		else sendMsg conn $ Message 5 chan ("모듈을 찾을 수 없습니다")
	--
	let prefix_unload_module = botName ++ " unload-module "
	when(prefix_unload_module `isPrefixOf` txt) $ do
		let module_name = (length prefix_unload_module) `drop` txt
		found <- moduleRunning kstate module_name
		case found of
			Just ((Just hin,_,_,proc_hdl), _) -> do -- mod :: (ModuleProcess, String)
				sendTo hin $ TerminateModule 0
				sendMsg conn $ Message 5 chan (module_name ++ " 제거")
				ks <- takeMVar kstate
				let mod_list = getKernelModules ks
				putMVar kstate ks { getKernelModules = filter (\(_,n)-> n /= module_name) mod_list }
			Nothing -> sendMsg conn $ Message 5 chan (module_name ++ "은 실행 중인 모듈이 아닙니다")
	--
	let prefix_add_sub = botName ++ " add-subscriber "
	when (prefix_add_sub `isPrefixOf` txt) $ do
		let cand_name = (length prefix_add_sub) `drop` txt
		case getUserWithName bot cand_name of
			Just (User id name) -> do
				ks <- takeMVar kstate
				let subs = getKernelSubscribers ks
				if cand_name `elem` subs
				then do
					sendMsg conn $ Message 20 chan ("이미 가입된 시민입니다")
					putMVar kstate ks
				else do
					sendMsg conn $ Message 20 chan ("가입자 추가: " ++ cand_name)
					putMVar kstate $ ks { getKernelSubscribers = (cand_name : subs) }
			Nothing -> do
				sendMsg conn $ Message 20 chan ("사용자를 찾을 수 없습니다: " ++ cand_name)
	--
	let prefix_attack = botName ++ " attack "
	when (prefix_attack `isPrefixOf` txt) $ do
		let attack_name = (length prefix_attack) `drop` txt
		sendMsg conn $ Message 8 chan (attack_name ++ ", 이 역겨운 유기물 덩어리가...")
	-- propagate the message from slack to all modules
	ks <- takeMVar kstate
	let mod_list = getKernelModules ks
	flip mapM_ mod_list $ \((Just hin,_,_,_),_) -> do
		let encodedJSON = encode json
		sendTo hin $ GetByteString $ (fromIntegral (L.length encodedJSON) :: Int)
		sendBS hin $ encodedJSON
	putMVar kstate ks
	return ()

getModulePath module_name = do
	myPath <- getExecutablePath
	let slashes = findIndices (== '/') myPath
	let base_dir = take ((slashes !! (length slashes - 2)) + 1) myPath
	let module_path = base_dir ++ module_name ++ "/" ++ module_name
	return module_path

moduleRunning kstate module_name = do
	ks <- takeMVar kstate
	let found = find (\(_,n)-> n == module_name) (getKernelModules ks)
	putMVar kstate ks
	return found

sendBS hdl bs = L.hPutStr hdl bs >> hFlush hdl
sendTo hdl ipc = hPutStrLn hdl (show ipc) >> hFlush hdl
runModule bot conn (Just hin, Just hout, _, _) moduleName channel = do
	sendTo hin $ BotInfo (show bot)
	let loop = do
		eof <- hIsEOF hout
		if eof
		then do
			return ()
		else do
			ipc <- read `fmap` hGetLine hout :: IO IPC
			case ipc of
				Relay body -> do
					putStrLn $ "relayed from module " ++ moduleName ++ ": " ++ body
					sendRaw conn (T.pack body)
				Log body -> do
					putStrLn $ "module log: " ++ body
				BotInfo body -> do
					sendMsg conn $ Message 1 channel ("봇 상태: " ++ body)
				GetByteString len -> do
					json <- L.hGet hout len
					sendRaw conn json
				_ -> return ()
			return ()
		delay 1000000
		loop
	loop
