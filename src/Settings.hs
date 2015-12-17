module Settings where

import System.IO
import qualified Data.ByteString.Char8 as B8

readToken = do
	hdl <- openFile "token.txt" ReadMode
	B8.hGetLine hdl
