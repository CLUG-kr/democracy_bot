module Main where

import Control.Concurrent.Thread.Delay
import Control.Concurrent
import Control.Monad
import System.Exit
import System.IO
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
			forkIO $ inputLogic (read info :: Bot)
			outputLogic
		_ -> send (Log "give me bot information") >> undefined

inputLogic bot = do
	send $ Log "Before input"
	ipc <- read `fmap` getLine :: IO IPC
	send $ Log "after input"
	case ipc of
		TerminateModule code -> do
			send $ Log "모듈을 종료합니다..."
			exitWith ExitSuccess
			hPutStrLn stderr "문제문제문제"
		GetByteString length -> do
			send $ Log "JSON 받았다!!!!"
			cont <- L.hGet stdin length
			let maybeJson = decode cont :: Maybe Object
			case maybeJson of
				Just json -> jsonHandler bot json
				Nothing -> send $ Log "json 파싱 실패"
		_ -> return ()
	delay 33333 --30fps
	inputLogic bot

jsonHandler bot json = do
	let msgType = getPropParser json "type" :: Result String
	case msgType of
		Success "message" -> do
			if hasProp json "subtype" then return ()
			else messageHandler bot json
		_ -> send $ Log "test module - unrecognized message"

messageHandler bot json = do
	let msg = parseMessage json
	let chan = receiveMessage_channel msg
	let txt = receiveMessage_text msg
	let userID = receiveMessage_user msg
	let userName = getUserName $ getUserWithID bot userID
	let botName = (getSelfName . getSelf) bot
	let botID = (getSelfID . getSelf) bot
	when (botName `isPrefixOf` txt) $ do
		let respond = Aeson.encode $ Message 1 chan ("did you call me, " ++ userName)
		send $ GetByteString $ (fromIntegral (L.length respond) :: Int)
		sendBS respond
	return ()

outputLogic = do
	send $ Log "ping from test module"
	--send $ Relay "{\"type\":\"message\", \"id\":1, \"channel\":\"C0GDE81EZ\", \"text\":\"message from <test> module\"}"
	delay 1000000
	outputLogic

sendBS bs = L.putStr bs >> hFlush stdout
send ipc = (putStrLn $ show ipc) >> hFlush stdout
