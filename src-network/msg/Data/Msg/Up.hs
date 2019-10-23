{-# LANGUAGE TemplateHaskell #-}
module Data.Msg.Up (module Data.Msg.Up, Table) where

import FlatBuffers

$(mkFlatBuffers "schemas/up.fbs" defaultOptions)