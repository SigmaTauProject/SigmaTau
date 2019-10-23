module Network.TerminalConnection where

import Control.Concurrent.STM.TChan

import Data.Msg.Up (Table, UpMsg)

data Connection = Connection (TChan (Table UpMsg))


