{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances #-}
----{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module World where

import Control.Monad ((<=<))

import Foreign.C
import Foreign.C.Types
import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

import Linear.Affine
import Linear.V3
import Linear.Quaternion

import Data.IORef

newtype World = World (ForeignPtr ())
type ERefFFI = Word32
newtype EntityRef = EntityRef ERefFFI

data EntityType = TypeAsteroid | TypeStation | TypeShip | TypeMissile
	deriving (Enum,Show)

foreign import ccall "newWorld" newWorldFFI :: IO (Ptr ())
foreign import ccall "&destroyWorld" destroyWorldFFIPtr :: FunPtr (Ptr () -> IO())
foreign import ccall "updateWorld" updateWorldFFI :: Ptr() -> IO ()
foreign import ccall "newEntity" newEntityFFI :: Ptr() -> Int32 -> Int32 -> Int32 -> Int32 -> IO (ERefFFI)

foreign import ccall "moveEntity" moveEntityFFI :: Ptr() -> ERefFFI -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall "forceEntity" forceEntityFFI :: Ptr() -> ERefFFI -> CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall "rotateEntity" rotateEntityFFI :: Ptr() -> ERefFFI -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall "angularForceEntity" angularForceEntityFFI :: Ptr() -> ERefFFI -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall "angularEulerForceEntity" angularEulerForceEntityFFI :: Ptr() -> ERefFFI -> CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall "getEntityPos" getEntityPosFFI :: Ptr() -> ERefFFI -> ERefFFI -> IO (Ptr (V3 CFloat))
foreign import ccall "getEntityOri" getEntityOriFFI :: Ptr() -> ERefFFI -> ERefFFI -> IO (Ptr (Quaternion CFloat))
type DoEntitiesCallback = ERefFFI->IO ()
foreign import ccall "wrapper" mkDoEntitiesCallback :: DoEntitiesCallback -> IO (FunPtr DoEntitiesCallback)
foreign import ccall "doEntities" doEntitiesFFI :: Ptr() -> FunPtr DoEntitiesCallback -> IO ()

newWorld :: IO World
newWorld = do
	world <- newWorldFFI
	World <$> newForeignPtr destroyWorldFFIPtr world
updateWorld :: World -> IO ()
updateWorld (World world) = withForeignPtr world (\wld->updateWorldFFI wld)

newEntity :: World -> EntityType -> Point V3 Int32 -> IO EntityRef
newEntity (World world) entityType (P (V3 x y z)) = EntityRef <$> withForeignPtr world (\wld->newEntityFFI wld (fromIntegral $ fromEnum entityType) x y z)

_moveEntity :: World -> EntityRef -> V3 Float -> IO ()
_moveEntity (World world) (EntityRef entityRef) (V3 x y z) = withForeignPtr world (\wld->moveEntityFFI wld entityRef (CFloat x) (CFloat y) (CFloat z))
_rotateEntity :: World -> EntityRef -> Quaternion Float -> IO ()
_rotateEntity (World world) (EntityRef entityRef) (Quaternion w (V3 x y z)) = withForeignPtr world (\wld->rotateEntityFFI wld entityRef (CFloat w) (CFloat x) (CFloat y) (CFloat z))

forceEntity :: World -> EntityRef -> V3 Float -> IO ()
forceEntity (World world) (EntityRef entityRef) (V3 x y z) = withForeignPtr world (\wld->forceEntityFFI wld entityRef (CFloat x) (CFloat y) (CFloat z))
angularForceEntity :: World -> EntityRef -> Float -> V3 Float -> IO ()
angularForceEntity (World world) (EntityRef entityRef) a (V3 x y z) = withForeignPtr world (\wld->angularForceEntityFFI wld entityRef (CFloat a) (CFloat x) (CFloat y) (CFloat z))
angularXForceEntity :: World -> EntityRef -> Float -> IO ()
angularXForceEntity world entityRef a = angularForceEntity world entityRef a (V3 1 0 0)
angularYForceEntity :: World -> EntityRef -> Float -> IO ()
angularYForceEntity world entityRef a = angularForceEntity world entityRef a (V3 0 1 0)
angularZForceEntity :: World -> EntityRef -> Float -> IO ()
angularZForceEntity world entityRef a = angularForceEntity world entityRef a (V3 0 0 1)
angularEulerForceEntity :: World -> EntityRef -> V3 Float -> IO ()
angularEulerForceEntity (World world) (EntityRef entityRef) (V3 x y z) = withForeignPtr world (\wld->angularEulerForceEntityFFI wld entityRef (CFloat x) (CFloat y) (CFloat z))

getEntityPos :: World -> EntityRef -> EntityRef -> IO (Point V3 Float)
getEntityPos (World world) (EntityRef rootEntityRef) (EntityRef entityRef) = withForeignPtr world (\wld->P . fmap (\(CFloat f)->f) <$> (peek =<< getEntityPosFFI wld rootEntityRef entityRef))
getEntityOri :: World -> EntityRef -> EntityRef -> IO (Quaternion Float)
getEntityOri (World world) (EntityRef rootEntityRef) (EntityRef entityRef) = withForeignPtr world (\wld->fmap (\(CFloat f)->f) <$> (peek =<< getEntityOriFFI wld rootEntityRef entityRef))

doEntities :: World -> (EntityRef->IO()) -> IO ()
doEntities (World world) callback = withForeignPtr world (\wld->(\fp->doEntitiesFFI wld fp >> freeHaskellFunPtr fp) =<< mkDoEntitiesCallback (\er->callback $ EntityRef er))

----withEntities :: World -> ([EntityRef] -> IO ()) -> IO ()
----withEntities world f -> do
----	f entities
----	doEntities world (\entity)
----	where entities = []

concatMapEntities :: World -> (EntityRef->IO [a]) -> IO [a]
concatMapEntities world callback = do
	rdsRef <- newIORef []
	doEntities world (\entity->do
			rd <- callback entity
			modifyIORef rdsRef (rd ++)
		)
	readIORef rdsRef
mapEntities :: World -> (EntityRef->IO a) -> IO [a]
mapEntities world callback = do
	rdsRef <- newIORef []
	doEntities world (\entity->do
			rd <- callback entity
			modifyIORef rdsRef (rd :)
		)
	readIORef rdsRef

----instance Storable (V3 Int32) where
----	sizeOf _ = 3 * sizeOf (undefined::Int32)
----	peek ptr = do
----		x <- peekByteOff ptr 0 :: IO Int32
----		y <- peekByteOff ptr (sizeOf x :: Int)
----		z <- peekByteOff ptr (sizeOf x + sizeOf y)
----		return $ V3 x y z
----instance Storable (Int32,Int32,Int32) where
----	sizeOf _ = 3 * sizeOf (undefined::Int32)
----	peek ptr = do
----		x <- peekByteOff ptr 0 :: IO Int32
----		y <- peekByteOff ptr (sizeOf x :: Int)
----		z <- peekByteOff ptr (sizeOf x + sizeOf y)
----		return (x,y,z)

