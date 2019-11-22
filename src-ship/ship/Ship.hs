{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ship (newShip) where

import Linear.Affine
import Linear.Vector ((*^))
import Linear.V3
import Linear.Quaternion

import Data.List (foldl')
import Data.IORef
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan

import World

import Network.TerminalConnection
import Data.Lifetime
import Data.Lifetime.Set

import Data.Word

import Data.ByteString.Lazy (append)
import qualified Data.ByteString.Lazy as BS
import Data.Serialize.Put (runPutLazy,putWord32le)
import Data.Serialize.Get (runGetLazy,getWord32le)
import FlatBuffers
import qualified FlatBuffers.Vector as FB
import Data.Msg.Common as Common
import Data.Msg.Bridge as Bridge
import Data.Msg.Wire as Wire
import Data.Msg.RadarArc as RadarArc
import Data.Msg.HackEV as HackEV

type NetworkConnection = (TChan (Lifetime Connection))

data Thruster = Thruster	{ thrusterPower :: IORef Float
	, thrusterEffect :: V3 Float
	, thrusterAngularEffect :: (Float, V3 Float)
	}

makeThruster effect angularEffect = Thruster <$> newIORef 0 <*> pure effect <*> pure angularEffect

newShip :: World -> NetworkConnection -> IO (IO ())
newShip world networkConnection = do
	activeConnections <- newLifetimeSet
	entity <- newEntity world TypeShip (P $ V3 0 0 0)
	_moveEntity world entity (V3 1 0 0)
	----angularZForceEntity world entity 0.8
	----angularYForceEntity world entity 0.0
	
	thrusters <- sequence $ [makeThruster (V3 1 0 0) (0, V3 0 0 1), makeThruster (V3 0 0 0) (0.01, V3 0 0 1)]
	
	return $ do
		forTChan networkConnection $ (\con -> do
				putStrLn "Con!"
				add activeConnections con
				with_ con (\(Connection upMsgChan downMsgChan) -> do
						atomically $ writeTChan downMsgChan $ DownMsg
							$ append (runPutLazy $ putWord32le 0)
							$ encode
							$ Bridge.downMsg
							$ Bridge.downMsgContentAddPorts
							$ Bridge.addPorts
							$ Just $ FB.fromList'
							$ map Bridge.fromPortType
							$ (take 2 $ repeat $ Bridge.PortTypeWire)
							++ [Bridge.PortTypeRadarArc]
							++ [Bridge.PortTypeHackEV]
					)
			)
		
		rawEntities <- mapEntities world (\e->(,) <$> getEntityPos world entity e <*> getEntityOri world entity e)
		let entitiesMsg	= append (runPutLazy $ putWord32le 4)
			$ encode
			$ HackEV.downMsg
			$ HackEV.downMsgContentUpdate $ HackEV.update
			$ Just $ FB.fromList'
			$ fmap (\(pos,ori)->HackEV.entity
					(Just $ toNetVec $ unP pos)
					(Just $ toNetQuat $ ori)
					(Just $ 0)
				)
			$ rawEntities
		let radarMsg	= append (runPutLazy $ putWord32le 3)
			$ encode
			$ RadarArc.downMsg
			$ RadarArc.downMsgContentUpdate $ RadarArc.update
			$ Just $ FB.fromList'
			$ fmap (\(pos,ori)->toNetVec $ unP pos)
			$ rawEntities
		withAll activeConnections (\con@(Connection chan downMsgChan)->do
				forTChan chan (\(UpMsg wholeMsg)->sequence_ $ do
						portID <- runGetLazy getWord32le wholeMsg
						let msg = BS.drop 4 wholeMsg
						case portID of
							1 -> do
								content <- Wire.upMsgContent =<< decode msg
								case content of
									Union (Wire.UpMsgContentSet wireSet) -> do
										value <- unnetworkFloat <$> Wire.setValue wireSet
										return $ do
											print value
											sequence_ $ writeIORef <$> (thrusterPower <$> thrusters !? 0) <*> pure value
									Union (Wire.UpMsgContentAdjust wireAdjust) -> do
										value <- unnetworkFloat <$> Wire.adjustValue wireAdjust
										return $ do
											print value
											sequence_ $ modifyIORef' <$> (thrusterPower <$> thrusters !? 0) <*> pure (\tv->tv + value)
							2 -> do
								content <- Wire.upMsgContent =<< decode msg
								case content of
									Union (Wire.UpMsgContentSet wireSet) -> do
										value <- unnetworkFloat <$> Wire.setValue wireSet
										return $ do
											print value
											sequence_ $ writeIORef <$> (thrusterPower <$> thrusters !? 1) <*> pure value
									Union (Wire.UpMsgContentAdjust wireAdjust) -> do
										value <- unnetworkFloat <$> Wire.adjustValue wireAdjust
										return $ do
											print value
											sequence_ $ modifyIORef' <$> (thrusterPower <$> thrusters !? 1) <*> pure (\tv->tv + value)
							_ -> return $ return ()
					)
				atomically $ writeTChan downMsgChan $ DownMsg entitiesMsg
				atomically $ writeTChan downMsgChan $ DownMsg radarMsg
			)
		
		forceEntity world entity =<< foldl' (+) (V3 0 0 0)
			<$> (sequence
				$ (\(Thruster powerRef effect _)->(*^ effect) <$> readIORef powerRef)
				<$> thrusters
			)
		sequence_ =<< fmap (uncurry $ angularForceEntity world entity)
			<$> (sequence
				$ (\(Thruster powerRef _ (a, v))->(\p->(p*a, v)) <$> readIORef powerRef)
				<$> thrusters
			)
		----forceEntity world entity =<< V3 <$> (truncate . (*64) <$> readIORef thrusterValueRef) <*> (truncate . (*64) <$> readIORef thrusterValueRef2) <*> pure 0
	
	


forTChan :: TChan a -> (a->IO ()) -> IO ()
forTChan chan f = theDo =<< (atomically $ tryReadTChan chan)
	where	theDo (Nothing) = return ()
		theDo (Just v) = f v >> forTChan chan f


networkFloat :: forall a. (Integral a, Bounded a) => Float -> a
networkFloat value
	| (minBound :: a) >= 0 = truncate $ min 1 value * fromIntegral (maxBound :: a)
	| otherwise = truncate $ max (fromIntegral (minBound :: a)) $ min 1 value * fromIntegral (maxBound :: a)
unnetworkFloat :: forall a. (Integral a, Bounded a) => a -> Float
unnetworkFloat value
	| (minBound :: a) >= 0 = fromIntegral value / fromIntegral (maxBound :: a)
	| otherwise = max (-1) $ fromIntegral value / fromIntegral (maxBound :: a)

toNetVec (V3 x y z) = vec3 x y z
toNetQuat (Quaternion w (V3 x y z)) = quaternion (networkFloat w) (networkFloat x) (networkFloat y) (networkFloat z)


infixl 9 !?
(!?) :: [a] -> Int -> Maybe a
(!?) xs o
	| o < 0 = Nothing
	| otherwise = f o xs
	where	f 0 (x:xs) = Just x
		f i (x:xs) = f (i-1) xs
		f i [] = Nothing





