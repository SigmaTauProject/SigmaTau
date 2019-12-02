{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ship (newShip) where

import Linear.Affine
import Linear.Vector ((*^))
import Linear.V3
import Linear.Quaternion

import Data.List (foldl')
import Data.IORef
import Control.Monad (void, forever)
import Control.Concurrent (forkIO)
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
	, thrusterAngularEffect :: V3 Float
	}

makeThruster effect angularEffect = Thruster <$> newIORef 0 <*> pure effect <*> pure angularEffect

newShip :: World -> NetworkConnection -> IO (IO ())
newShip world networkConnection = do
	activeConnections <- newLifetimeSet
	entity <- newEntity world TypeShip (P $ V3 0 0 0)
	_moveEntity world entity (V3 1 0 0)
	----angularZForceEntity world entity 0.8
	----angularYForceEntity world entity 0.0
	
	thrusters <- sequence $	[ makeThruster (V3 0.5 0 0) (V3 0 0 0)
		, makeThruster (V3 0 0.5 0) (V3 0 0 0)
		, makeThruster (V3 0 0 0.5) (V3 0 0 0)
		, makeThruster (V3 0 0 0) (V3 0.05 0 0)
		, makeThruster (V3 0 0 0) (V3 0 0.05 0)
		, makeThruster (V3 0 0 0) (V3 0 0 0.05)
		]
	
	commandChan <- atomically newTChan
	
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
							$ []
							++ (take 6 $ repeat $ Bridge.PortTypeWire)
							++ [Bridge.PortTypeRadarArc]
							++ [Bridge.PortTypeHackEV]
					)
			)
		
		rawEntities <- mapEntities world (\e->(,) <$> getEntityPos world entity e <*> getEntityOri world entity e)
		let entitiesMsg	= append (runPutLazy $ putWord32le 8)
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
		let radarMsg	= append (runPutLazy $ putWord32le 7)
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
							assumeWirePortID -> do
								content <- Wire.upMsgContent =<< decode msg
								case content of
									Union (Wire.UpMsgContentSet wireSet) -> do
										value <- unnetworkFloat <$> Wire.setValue wireSet
										return $ do
											sequence_ $ writeIORef <$> (thrusterPower <$> thrusters !? fromIntegral (assumeWirePortID-1)) <*> pure ((min 1 . max (-1)) value)
									Union (Wire.UpMsgContentAdjust wireAdjust) -> do
										value <- unnetworkFloat <$> Wire.adjustValue wireAdjust
										return $ do
											sequence_ $ modifyIORef' <$> (thrusterPower <$> thrusters !? fromIntegral (assumeWirePortID-1)) <*> pure (\tv->(min 1 . max (-1)) (tv + value))
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
		sequence_ =<< fmap (angularForceEntity world entity)
			<$> (sequence
				$ (\(Thruster powerRef _ a)->(\p->p*^a) <$> readIORef powerRef)
				<$> thrusters
			)
		forkIO $ forever $ do
			command <- getLine
			atomically $ writeTChan commandChan command
		forTChan commandChan (\command-> case command of
				"" -> return ()
				"=m" -> _moveEntity world entity (V3 1 0 0)
				"=n" -> _moveEntity world entity (V3 0 1 0)
				"=o" -> _moveEntity world entity (V3 0 0 1)
				"=r" -> _rotateEntity world entity $ axisAngle (V3 1 0 0) (pi/32)
				"=s" -> _rotateEntity world entity $ axisAngle (V3 0 1 0) (pi/32)
				"=t" -> _rotateEntity world entity $ axisAngle (V3 0 0 1) (pi/32)
				"=-m" -> _moveEntity world entity (V3 (-1) 0 0)
				"=-n" -> _moveEntity world entity (V3 0 (-1) 0)
				"=-o" -> _moveEntity world entity (V3 0 0 (-1))
				"=-r" -> _rotateEntity world entity $ axisAngle (V3 1 0 0) (-pi/23)
				"=-s" -> _rotateEntity world entity $ axisAngle (V3 0 1 0) (-pi/32)
				"=-t" -> _rotateEntity world entity $ axisAngle (V3 0 0 1) (-pi/32)
				"m" -> forceEntity world entity (V3 1 0 0)
				"n" -> forceEntity world entity (V3 0 1 0)
				"o" -> forceEntity world entity (V3 0 0 1)
				"r" -> angularForceEntity world entity (V3 (pi/32) 0 0)
				"s" -> angularForceEntity world entity (V3 0 (pi/32) 0)
				"t" -> angularForceEntity world entity (V3 0 0 (pi/32))
				"-m" -> forceEntity world entity (V3 (-1) 0 0)
				"-n" -> forceEntity world entity (V3 0 (-1) 0)
				"-o" -> forceEntity world entity (V3 0 0 (-1))
				"-r" -> angularForceEntity world entity (V3 (-pi/32) 0 0)
				"-s" -> angularForceEntity world entity (V3 0 (-pi/32) 0)
				"-t" -> angularForceEntity world entity (V3 0 0 (-pi/32))
				_ -> putStrLn "invalid"
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





