module IPC where

data IPC =
	TerminateModule Int
	| BotInfo String
	| Relay String
	| Log String
	| GetByteString Int
	deriving (Show, Read)
