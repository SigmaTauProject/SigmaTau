{-# LANGUAGE TemplateHaskell #-}
module Msg.HackEV (module Msg.HackEV) where

import FlatBuffers

import Msg.Common

$(mkFlatBuffers "schemas/hackEV.fbs" defaultOptions)