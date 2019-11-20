{-# LANGUAGE TemplateHaskell #-}
module Msg.Wire (module Msg.Wire) where

import FlatBuffers

import Msg.Common

$(mkFlatBuffers "schemas/wire.fbs" defaultOptions)