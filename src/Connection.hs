{-# LANGUAGE OverloadedStrings #-}

module Connection where

import Network.HTTP.Conduit
import Network.HTTP.Client.TLS
import qualified Network.WebSockets as WS
import Wuss

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Control.Monad.Trans

import Bot

rtmStart slackToken = do
	putStrLn "send request for rtm.start..."
	-- current state of the slack team (JSON formatted)
	-- "url" field contains the websocket server url we need
	content <- slackWebAPI "rtm.start" [("token",slackToken)]
	putStrLn $ "response received - total " ++ (show $ L.length content) ++ " characters"
	return $ Aeson.decode content

-- call the slack API using HTTPS
slackWebAPI :: String -> [(B.ByteString, B.ByteString)] -> IO L.ByteString
slackWebAPI method args = do
	man <- newManager tlsManagerSettings
	initReq <- parseUrl $ "https://slack.com/api/" ++ method
	let req' = initReq { secure = True }
	let req = urlEncodedBody args req'
	response <- httpLbs req man
	return $ responseBody response

runBot slackToken fail success = do
	bot <- rtmStart slackToken
	case bot of
		Nothing -> fail
		Just bot -> do
			putStrLn "trying to connect..."
			Wuss.runSecureClient (getServerHost bot) 443 (getServerPath bot) (success bot)

sendRaw conn rawMsg = WS.sendTextData conn rawMsg
sendMsg conn msg = WS.sendTextData conn (Aeson.encode msg)

recvMsg :: WS.Connection -> IO (Maybe Aeson.Object)
recvMsg conn = do
	msg <- WS.receiveData conn :: IO T.Text
	liftIO $ T.putStrLn msg
	return (Aeson.decode $ L.fromStrict $ T.encodeUtf8 msg :: Maybe Aeson.Object)

