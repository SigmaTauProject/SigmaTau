{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Linear.Affine
import Linear.V3

import Control.Monad (forever)
import Control.Concurrent (threadDelay)

import DRuntime

import World
import Ship

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan

import Network.TerminalServer
import Data.Lifetime
import Network.TerminalConnection

import FlatBuffers
import Data.Msg.Up as UM


main = withDRuntime $ do
	world <- newWorld
	entity1 <- newEntity world TypeShip (P $ V3 (-50) 1 0)
	newEntity world TypeShip (P $ V3 6 5 (-3))
	newEntity world TypeShip (P $ V3 9 8 2)
	newEntity world TypeShip (P $ V3 9 9 10)
	newEntity world TypeShip (P $ V3 (-1) (-1) 0)
	forceEntity world entity1 (V3 10 0 0)
	
	connectionChan <- runTerminalServer
	
	----connectionChan <- atomically newTChan
	ship <- newShip world connectionChan
	msgChan <- atomically newTChan
	----atomically $ writeTChan connectionChan $ Lifetime $ return $ Just $ Connection $ msgChan
	----sequence_ $ atomically <$> (writeTChan msgChan <$> (decode $ encode $ upMsg $ msgContentSetThruster $ setThruster (Just 0) (Just 2)))
	----getLine
	----ship
	----sequence_ $ atomically <$> (writeTChan msgChan <$> (decode $ encode $ upMsg $ msgContentSetThruster $ setThruster (Just 0) (Just 2)))
	----getLine
	----ship
	----sequence_ $ atomically <$> (writeTChan msgChan <$> (decode $ encode $ upMsg $ msgContentAdjustThruster $ adjustThruster (Just 0) (Just (-8))))
	
	
	forever (threadDelay 50000>>updateWorld world>>ship)






















































