module Main where

import Control.Concurrent.Thread.Delay
import Control.Concurrent
import Control.Monad
import System.Exit
import System.IO
import Data.Int
import Data.Aeson as Aeson
import Data.Aeson.Types as Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import IPC

main = do
	hSetBuffering stdin LineBuffering
	hSetBuffering stderr LineBuffering
	hSetBuffering stdout LineBuffering
	forkIO $ inputLogic
	outputLogic
	
inputLogic = forever $ do
	send $ Log "Before input"
	ipc <- read `fmap` getLine :: IO IPC
	send $ Log "after input"
	case ipc of
		TerminateModule code -> do
			send $ Log "모듈을 종료합니다..."
			exitWith ExitSuccess
			hPutStrLn stderr "문제문제문제"
		GetByteString length -> do
			cont <- L.hGet stdin length
			let maybeJson = decode cont :: Maybe Object
			case maybeJson of
				Just json -> do
					let typ = case flip Aeson.parse json $ \v -> v .: (T.pack "type") :: Aeson.Parser String of
						Success s -> s
						Error _ -> "err"
					send $ Log $ "json 받았다: " ++ typ
				Nothing -> send $ Log "json 파싱 실패"
		_ -> return ()

outputLogic = do
	send $ Log "로그라이크"
	--send $ Relay "{\"type\":\"message\", \"id\":1, \"channel\":\"C0GDE81EZ\", \"text\":\"message from <test> module\"}"
	delay 1000000
	outputLogic

send ipc = (putStrLn $ show ipc) >> hFlush stdout
