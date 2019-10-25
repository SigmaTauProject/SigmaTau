module Network.TerminalConnection where

import Control.Concurrent.STM.TChan

import Data.Msg.Up (Table, UpMsg)
import Data.Msg.Down (WriteTable, DownMsg)

data Connection = Connection (TChan (Table UpMsg)) (TChan (WriteTable DownMsg))


