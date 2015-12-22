{-# LANGUAGE OverloadedStrings #-}

module Bot where

import Data.Maybe (fromJust)
import Data.List (find)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (forM, liftM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson ((.=), FromJSON, parseJSON, ToJSON, toJSON, object, encode, decode)
import Data.Aeson.Types ((.:))
import Text.Regex.Posix ((=~))

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

-- parsing utils
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

getUserWithName bot userName = find f (getUsers bot) where
	f (User id name) = name == userName

parseMessage json = ReceiveSimpleMessage {
	receiveMessage_channel = getProp json "channel" :: String,
	receiveMessage_user = getProp json "user" :: String,
	receiveMessage_text = getProp json "text" :: String
}

