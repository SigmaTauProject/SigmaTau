{-# LANGUAGE ForeignFunctionInterface #-}
module DRuntime where

foreign import ccall "rt_init" dRuntimeInit :: IO Bool
foreign import ccall "rt_term" dRuntimeTerm :: IO Bool

withDRuntime :: IO a -> IO a
withDRuntime callback = do
	dRuntimeInit
	rd <- callback
	dRuntimeTerm
	return $ rd

