{-# LANGUAGE TupleSections #-}
module Ship (newShip) where

import Linear.Affine
import Linear.V3

import Data.IORef
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan

import World

import Network.TerminalConnection
import Data.Lifetime
import Data.Lifetime.Set

import Data.Word

import FlatBuffers
import Data.Msg.Up as UM

type NetworkConnection = (TChan (Lifetime Connection))

newShip :: World -> NetworkConnection -> IO (IO ())
newShip world networkConnection = do
	activeConnections <- newLifetimeSet
	entity <- newEntity world (P $ V3 0 0 0)
	thrusterValueRef <- newIORef 0
	return $ do
		forTChan networkConnection $ (\con->do
				putStrLn "Con!"
				add activeConnections con
			)
		
		withAll activeConnections (\con@(Connection chan)->do
				forTChan chan (\msg->sequence_ $ do
						content <- upMsgContent msg
						case content of
							Union (MsgContentSetThruster setThruster) -> do
								id <- setThrusterId setThruster
								value <- setThrusterValue setThruster
								return $ do
									print id
									print value
									writeIORef thrusterValueRef value
							Union (MsgContentAdjustThruster adjustThruster) -> do
								id <- adjustThrusterId adjustThruster
								value <- adjustThrusterValue adjustThruster
								return $ do
									print id
									print value
									modifyIORef' thrusterValueRef (\tv->tv + value)
					)
			)
		
		
		forceEntity world entity =<< V3 <$> (fromIntegral <$> readIORef thrusterValueRef) <*> pure 0 <*> pure 0
	
	


forTChan :: TChan a -> (a->IO ()) -> IO ()
forTChan chan f = theDo =<< (atomically $ tryReadTChan chan)
	where	theDo (Nothing) = return ()
		theDo (Just v) = f v >> forTChan chan f





