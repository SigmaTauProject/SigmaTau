{-# LANGUAGE OverloadedStrings #-}
module Network.TerminalServer (runTerminalServer) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Data.Text

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static
import Network.Wai.Handler.WebSockets
import Network.WebSockets

import Network.Terminal (runTerminal)

-----import Data.Msg.Up
----import Msg.Down

import Data.Lifetime
import qualified Network.TerminalConnection as Terminal (Connection)

runTerminalServer :: IO (TChan (Lifetime Terminal.Connection))
runTerminalServer = do
	let port = 8951
	putStrLn $ "Listening on port " ++ show port
	let app = staticApp $ defaultWebAppSettings "www"
	newConChan <- atomically $ newTChan
	let wsApp = websocketsOr defaultConnectionOptions (\pc->acceptRequest pc >>= runTerminal >>= atomically . writeTChan newConChan) app
	forkIO $ run port wsApp
	return newConChan











