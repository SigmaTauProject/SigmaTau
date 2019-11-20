{-# LANGUAGE TemplateHaskell #-}
module Msg.RadarArc (module Msg.RadarArc) where

import FlatBuffers

import Msg.Common

$(mkFlatBuffers "schemas/radarArc.fbs" defaultOptions)