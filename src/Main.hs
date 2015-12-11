import Control.Concurrent (forkIO)
import Control.Monad
import Control.Monad.Trans

import Data.List

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson as Aeson
import Data.Aeson.Types

import qualified Settings
import Bot

main = runBot Settings.token cannotConnect kernel

cannotConnect = do
	putStrLn "cannot retrieve a websocket url. exits..."

kernel bot conn = do
	putStrLn "Connected to clug slack"
	putStrLn $ "bot name: " ++ (getSelfName $ getSelf bot)
	putStrLn $ "bot id:   " ++ (getSelfID $ getSelf bot)
	--sendMsg conn (Message 1 "C04TJBLCG" "kernel loaded")
	_ <- forkIO $ forever $ do
		recvMsg conn >>= handleInput bot conn
	let loop = do
		line <- T.getLine
		unless (T.null line) $ sendRaw conn line >> loop
	loop

handleInput bot conn jsonMsg = case jsonMsg of
	Just json -> do
		let msgType = getPropParser json "type" :: Result String
		case msgType of
			Success "message" -> handleInput_message bot conn json
			_ -> putStrLn "=> unrecognized message"
	Nothing -> do
		putStrLn "slack sent me non-json data... wtf?"
		putStrLn $ "=> " ++ (show jsonMsg)

parseMessage json = ReceiveMessage {
	receiveMessage_channel = getProp json "channel" :: String,
	receiveMessage_user = getProp json "user" :: String,
	receiveMessage_text = getProp json "text" :: String
}

handleInput_message bot conn json = do
	let msg = parseMessage json
	let chan = receiveMessage_channel msg
	let usr = receiveMessage_user msg
	let txt = receiveMessage_text msg
	let usrName = getUserName $ getUserWithID bot usr
	let botName = (getSelfName . getSelf) bot
	let botID = (getSelfID . getSelf) bot
	when (botName `isPrefixOf` txt) $ do
		sendMsg conn $ Message 1 chan ("did you call me, " ++ usrName)
	when (txt == botName ++ " status") $ do
		sendMsg conn $ Message 2 chan ("my name is " ++ botName ++ "\nmy id is " ++ botID)
		sendMsg conn $ Message 3 chan ("active modules: empty")
	when (txt == botName ++ " help") $ do
		sendMsg conn $ Message 3 chan ("나는 나보다 약한 녀석의 명령 따윈 듣지 않는다.")
	return ()
