{-# LANGUAGE OverloadedStrings #-}

module Bot where

import Data.Maybe (fromJust)
import Data.List (find)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (forM, liftM)
import Control.Monad.Trans
import Network.HTTP.Conduit
import Network.HTTP.Client.TLS
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString as B
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import Data.Aeson ((.=), FromJSON, parseJSON, ToJSON, toJSON, object, encode, decode)
import Data.Aeson.Types ((.:))
import Text.Regex.Posix ((=~))
import Wuss

data Self = Self {
	getSelfID :: String,
	getSelfName :: String
} deriving (Show, Read)

data User = User {
	getUserID :: String,
	getUserName :: String
} deriving (Show, Read)

-- data that need to be modified and retreived between threads
-- are not maintained here. see (MVar KernelState) in Main.hs
data Bot = Bot {
	getSelf :: Self,
	getUsers :: [User],
	getServerHost :: String,
	getServerPath :: String
} deriving (Show, Read)

instance FromJSON Self where
	parseJSON (Aeson.Object v) =
		Self <$> (v .: "id") <*> (v .: "name")

instance FromJSON User where
	parseJSON (Aeson.Object v) =
		User <$> (v .: "id") <*> (v .: "name")

instance FromJSON Bot where
	parseJSON (Aeson.Object v) = Bot <$> self <*> users <*> (pure host) <*> (pure path) where
		self = v .: "self" :: Aeson.Parser Self
		users = v .: "users" :: Aeson.Parser [User]
		Aeson.Success (host, path) = flip Aeson.parse v $ \obj -> do
			url <- obj .: "url" :: Aeson.Parser String
			let (h1, h2, h3) = url =~ (".com" :: String) :: (String, String, String)
			return (drop 6 $ h1++h2, h3)

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
sendMsg conn msg = WS.sendTextData conn (encode msg)

recvMsg :: WS.Connection -> IO (Maybe Aeson.Object)
recvMsg conn = do
	msg <- WS.receiveData conn :: IO T.Text
	liftIO $ T.putStrLn msg
	return (Aeson.decode $ L.fromStrict $ T.encodeUtf8 msg :: Maybe Aeson.Object)

-- from bot to slack
data SendMessage = Message {
	id :: Int,
	channel :: String,
	text :: String
} deriving Show

instance ToJSON SendMessage where
	toJSON (Message id chan txt) =
		object ["id" .= id, "channel" .= chan,
				"text" .= txt, "type" .= ("message" :: String)]

-- from slack to bot
data ReceiveMessage = ReceiveSimpleMessage {
	receiveMessage_channel :: String,
	receiveMessage_user :: String,
	receiveMessage_text :: String
} deriving Show

getProp :: Aeson.Object -> String -> String
getProp json prop = case getPropParser json prop of
	Aeson.Success v -> v
	Aeson.Error _ -> undefined

hasProp :: Aeson.Object -> String -> Bool
hasProp json prop = case getPropParser json prop of
	Aeson.Success v -> True
	Aeson.Error _ -> False

getPropParser :: Aeson.Object -> String -> Aeson.Result String
getPropParser json prop = flip Aeson.parse json $
	\obj -> obj .: (T.pack prop) :: Aeson.Parser String

getUserWithID bot userID = fromJust $ find f (getUsers bot) where
	f (User id name) = id == userID
