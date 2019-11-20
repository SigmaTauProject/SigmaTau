{-# LANGUAGE TemplateHaskell #-}
module Msg.Common (module Msg.Common) where

import FlatBuffers

$(mkFlatBuffers "schemas/common.fbs" defaultOptions)