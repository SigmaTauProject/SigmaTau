module Network.TerminalConnection where

import Data.ByteString.Lazy
import Control.Concurrent.STM.TChan

newtype UpMsg = UpMsg ByteString
newtype DownMsg = DownMsg ByteString

data Connection = Connection (TChan UpMsg) (TChan DownMsg)


