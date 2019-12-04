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

import Network.TerminalConnection hiding (Connection)
import qualified Network.TerminalConnection as Terminal (Connection(Connection))
import Data.Lifetime

runTerminal :: TChan UpMsg -> TChan DownMsg -> Connection -> IO ()
runTerminal upMsgChan downMsgChan connection = do
	putStrLn "New Connection"
	----sendChannel <- newChan
	----let sendPush = newPush (\m->print m>>writeChan sendChannel m)
	----FRP.send upMsgPush $ (sendPush, makeMsgLacking $ UpMsg (ComponentID 0) (BridgeUpMsg MBU.Connect))
	--reading thread
	forkIO $ do
		forever $ do
			dataMsg <- receiveDataMessage connection
			case dataMsg of
				(Binary d) -> atomically . writeTChan upMsgChan $ UpMsg d
				(Text _ _) -> return ()
	----let con = Terminal.Connection upMsgChan
	----return $ Lifetime (return $ Just $ con)
	--writing thread
	forever $ do
		(DownMsg msgData) <- atomically $ readTChan downMsgChan
		sendDataMessage connection $ Binary msgData
	----sequence_ =<< fmap (sendDataMessage connection . Binary . fromStrict . serializeDownMsg) <$> getChanContents sendChannel











