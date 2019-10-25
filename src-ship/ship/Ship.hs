{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
	thrusterValueRef2 <- newIORef 0
	return $ do
		forTChan networkConnection $ (\con->do
				putStrLn "Con!"
				add activeConnections con
			)
		
		withAll activeConnections (\con@(Connection chan)->do
				forTChan chan (\msg->sequence_ $ do
						content <- upMsgContent msg
						case content of
							Union (MsgContentWireSet wireSet) -> do
								id <- wireSetId wireSet
								value <- unnetworkFloat <$> wireSetValue wireSet
								return $ do
									print id
									print value
									if (id==0)
										then writeIORef thrusterValueRef value
										else writeIORef thrusterValueRef2 value
							Union (MsgContentWireAdjust wireAdjust) -> do
								id <- wireAdjustId wireAdjust
								value <- unnetworkFloat <$> wireAdjustValue wireAdjust
								return $ do
									print id
									print value
									if (id==0)
										then modifyIORef' thrusterValueRef (\tv->tv + value)
										else modifyIORef' thrusterValueRef2 (\tv->tv + value)
					)
			)
		
		
		forceEntity world entity =<< V3 <$> (truncate . (*64) <$> readIORef thrusterValueRef) <*> (truncate . (*64) <$> readIORef thrusterValueRef2) <*> pure 0
	
	


forTChan :: TChan a -> (a->IO ()) -> IO ()
forTChan chan f = theDo =<< (atomically $ tryReadTChan chan)
	where	theDo (Nothing) = return ()
		theDo (Just v) = f v >> forTChan chan f


unnetworkFloat :: forall a. (Integral a, Bounded a) => a -> Float
unnetworkFloat value
	| (minBound :: a) >= 0 = fromIntegral value / fromIntegral (maxBound :: a)
	| otherwise = max (-1) $ fromIntegral value / fromIntegral (maxBound :: a)




