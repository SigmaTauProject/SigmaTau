{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Network.Terminal (runTerminal) where

import Control.Monad (void, forever)

import Control.Concurrent (forkIO)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static
import Network.Wai.Handler.WebSockets
import Network.WebSockets

import Data.ByteString.Lazy (fromStrict, toStrict)

import FlatBuffers
import Data.Msg.Up
import Data.Msg.Down
import qualified Network.TerminalConnection as Terminal (Connection(Connection))
import Data.Lifetime

runTerminal :: TChan (Table UpMsg) -> TChan (WriteTable DownMsg) -> Connection -> IO ()
runTerminal upMsgChan downMsgChan connection = do
	putStrLn "New Connection"
	----sendChannel <- newChan
	----let sendPush = newPush (\m->print m>>writeChan sendChannel m)
	----FRP.send upMsgPush $ (sendPush, makeMsgLacking $ UpMsg (ComponentID 0) (BridgeUpMsg MBU.Connect))
	--reading thread
	forkIO $ do
		forever $ do
			dataMsg <- receiveDataMessage connection
			putStr "recieved: "
			print dataMsg
			case dataMsg of
				(Binary d) -> sequence_ $ atomically . writeTChan upMsgChan <$> decode d
				(Text _ _) -> return ()
	----let con = Terminal.Connection upMsgChan
	----return $ Lifetime (return $ Just $ con)
	--writing thread
	forever $ do
		msg <- atomically $ readTChan downMsgChan
		sendDataMessage connection (Binary $ encode msg)
	----sequence_ =<< fmap (sendDataMessage connection . Binary . fromStrict . serializeDownMsg) <$> getChanContents sendChannel











