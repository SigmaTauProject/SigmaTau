{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ship (newShip) where

import Linear.Affine
import Linear.Vector ((*^))
import Linear.V3

import Data.List (foldl')
import Data.IORef
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan

import World

import Network.TerminalConnection
import Data.Lifetime
import Data.Lifetime.Set

import Data.Word

import FlatBuffers
import qualified FlatBuffers.Vector as FB
import Data.Msg.Up as UM
import Data.Msg.Down as DM

type NetworkConnection = (TChan (Lifetime Connection))

data Thruster = Thruster	{ thrusterPower :: IORef Float
	, thrusterEffect :: V3 Float
	}

makeThruster effect = Thruster <$> newIORef 0 <*> pure effect

newShip :: World -> NetworkConnection -> IO (IO ())
newShip world networkConnection = do
	activeConnections <- newLifetimeSet
	entity <- newEntity world TypeShip (P $ V3 0 0 0)
	
	thrusters <- sequence $ [makeThruster (V3 1 0 0), makeThruster (V3 0 1 0)]
	
	return $ do
		forTChan networkConnection $ (\con->do
				putStrLn "Con!"
				add activeConnections con
			)
		
		let entitiesMsg = downMsg $ msgContentLAUpdate $ lAUpdate $ Just $ FB.fromList' $ [vec3 0 1 2]
		withAll activeConnections (\con@(Connection chan downMsgChan)->do
				forTChan chan (\msg->sequence_ $ do
						content <- upMsgContent msg
						case content of
							Union (MsgContentWireSet wireSet) -> do
								id <- wireSetId wireSet
								value <- unnetworkFloat <$> wireSetValue wireSet
								return $ do
									print id
									print value
									sequence_ $ writeIORef <$> (thrusterPower <$> thrusters !? fromIntegral id) <*> pure value
							Union (MsgContentWireAdjust wireAdjust) -> do
								id <- wireAdjustId wireAdjust
								value <- unnetworkFloat <$> wireAdjustValue wireAdjust
								return $ do
									print id
									print value
									sequence_ $ modifyIORef' <$> (thrusterPower <$> thrusters !? fromIntegral id) <*> pure (\tv->tv + value)
					)
				atomically $ writeTChan downMsgChan entitiesMsg
			)
		
		forceEntity world entity =<< foldl' (+) (V3 0 0 0) <$> (sequence $ (\(Thruster powerRef effect)->(*^ effect) <$> readIORef powerRef) <$> thrusters)
		----forceEntity world entity =<< V3 <$> (truncate . (*64) <$> readIORef thrusterValueRef) <*> (truncate . (*64) <$> readIORef thrusterValueRef2) <*> pure 0
	
	


forTChan :: TChan a -> (a->IO ()) -> IO ()
forTChan chan f = theDo =<< (atomically $ tryReadTChan chan)
	where	theDo (Nothing) = return ()
		theDo (Just v) = f v >> forTChan chan f


unnetworkFloat :: forall a. (Integral a, Bounded a) => a -> Float
unnetworkFloat value
	| (minBound :: a) >= 0 = fromIntegral value / fromIntegral (maxBound :: a)
	| otherwise = max (-1) $ fromIntegral value / fromIntegral (maxBound :: a)


infixl 9 !?
(!?) :: [a] -> Int -> Maybe a
(!?) xs o
	| o < 0 = Nothing
	| otherwise = f o xs
	where	f 0 (x:xs) = Just x
		f i (x:xs) = f (i-1) xs
		f i [] = Nothing

