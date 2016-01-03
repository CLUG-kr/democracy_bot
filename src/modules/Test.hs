module Main where

import Control.Concurrent.Thread.Delay
import Control.Concurrent
import Control.Monad
import System.Exit
import System.IO
import System.Process
import Data.Int
import Data.List
import Data.Aeson as Aeson
import Data.Aeson.Types as Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L

import Bot
import IPC

main = do
	hSetBuffering stdin LineBuffering
	hSetBuffering stderr LineBuffering
	hSetBuffering stdout LineBuffering
	botInput <- read `fmap` getLine :: IO IPC
	case botInput of
		BotInfo info -> do
			forkIO $ outputLogic
			inputLogic (read info :: Bot)
		_ -> send (Log "give me bot information") >> undefined

inputLogic bot = do
	ipc <- read `fmap` getLine :: IO IPC
	case ipc of
		TerminateModule code -> do
			send $ Log "모듈을 종료합니다..."
			exitWith ExitSuccess
			hPutStrLn stderr "[에러] 모듈에 문제 발생"
		GetByteString length -> do
			send $ Log "test 모듈 - JSON received!"
			cont <- L.hGet stdin length
			L.hGet stdin 1
			let maybeJson = decode cont :: Maybe Object
			case maybeJson of
				Just json -> jsonHandler bot json
				Nothing -> send $ Log "[에러] json 파싱 실패"
		_ -> return ()
	--delay 33333 --30fps
	inputLogic bot

jsonHandler bot json = do
	let msgType = getPropParser json "type" :: Result String
	case msgType of
		Success "message" -> do
			if hasProp json "subtype" then return ()
			else messageHandler bot json
		_ -> send $ Log "[에러] test module - unrecognized message"

messageHandler bot json = do
	let msg = parseMessage json
	let chan = receiveMessage_channel msg
	let txt = receiveMessage_text msg
	let userID = receiveMessage_user msg
	let userName = getUserName $ getUserWithID bot userID
	let botName = (getSelfName . getSelf) bot
	let botID = (getSelfID . getSelf) bot
	-- help
	when (txt == botName ++ " help") $ do
		let helpMsg = intercalate "\n" $
			["[test 모듈 명령어 목록]"
			,"shell <shell_cmd>: 셸에서 이 명령을 실행한다"]
		sendMsg $ Message 1 chan helpMsg
	-- shell
	whenPrefix (botName ++ " shell ") txt $ \cmd -> do
		sendMsg $ Message 10 chan ("셸 명령 실행: " ++ cmd)
		(_,Just hout,_,_) <- createProcess $ (shell cmd){ std_out = CreatePipe }
		shell_output <- hGetContents hout
		sendMsg $ Message 20 chan shell_output
	-- end of messageHandler
	return ()

whenPrefix pre txt action =
	let msg = (length pre) `drop` txt
	in when (pre `isPrefixOf` txt) $ action msg

outputLogic = do
	-- do nothing currently
	delay 1000000
	outputLogic

sendMsg msg = do
	let bs = Aeson.encode msg
	send (GetByteString $ (fromIntegral (L.length bs) :: Int))
	sendBS bs
sendBS bs = L.putStr bs >> hFlush stdout
send ipc = (putStrLn $ show ipc) >> hFlush stdout
