{-# LANGUAGE TemplateHaskell #-}
module Msg.Up (module Msg.Up) where

import FlatBuffers

$(mkFlatBuffers "schemas/up.fbs" defaultOptions)