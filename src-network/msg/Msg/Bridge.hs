{-# LANGUAGE TemplateHaskell #-}
module Msg.Bridge (module Msg.Bridge) where

import FlatBuffers

$(mkFlatBuffers "schemas/bridge.fbs" defaultOptions)

