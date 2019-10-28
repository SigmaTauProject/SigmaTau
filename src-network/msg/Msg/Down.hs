{-# LANGUAGE TemplateHaskell #-}
module Msg.Down (module Msg.Down) where

import FlatBuffers

$(mkFlatBuffers "schemas/down.fbs" defaultOptions)