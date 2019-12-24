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

----newtype LoosePtr a = LoosePtr (Ptr a)

data EntityType = TypeAsteroid | TypeStation | TypeShip | TypeMissile
	deriving (Enum,Show)


foreign import ccall "holdPtr" holdPtrFFI :: Ptr a -> IO () --Only necessary when hold a pointer which was not directly returned (a pointer which was in a returned pointer).
foreign import ccall "&releasePtr" releasePtrFFIPtr :: FunPtr (Ptr a -> IO ())
foreign import ccall "releasePtr" releasePtrFFI :: Ptr a -> IO ()
acceptPtr :: Ptr a -> IO (ForeignPtr a)
acceptPtr = newForeignPtr releasePtrFFIPtr
holdPtr :: Ptr a -> IO (ForeignPtr a)
holdPtr ptr = holdPtrFFI ptr >> acceptPtr ptr
usePtr :: Ptr a -> (Ptr a -> IO b) -> IO b
usePtr ptr callback = do
	rd <- callback ptr
	releasePtrFFI ptr
	return rd

foreign import ccall "newWorld" newWorldFFI :: IO (Ptr ())
foreign import ccall "updateWorld" updateWorldFFI :: Ptr () -> IO ()
foreign import ccall "newEntity" newEntityFFI :: Ptr () -> Int32 -> Int32 -> Int32 -> Int32 -> IO (ERefFFI)

foreign import ccall "locatedForceEntity" locatedForceEntityFFI :: Ptr () -> ERefFFI -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall "moveEntity" moveEntityFFI :: Ptr () -> ERefFFI -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall "forceEntity" forceEntityFFI :: Ptr () -> ERefFFI -> CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall "rotateEntity" rotateEntityFFI :: Ptr () -> ERefFFI -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall "angularForceEntity" angularForceEntityFFI :: Ptr () -> ERefFFI -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall "angularEulerForceEntity" angularEulerForceEntityFFI :: Ptr () -> ERefFFI -> CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall "getEntityPos" getEntityPosFFI :: Ptr () -> ERefFFI -> ERefFFI -> IO (Ptr (V3 CFloat))
foreign import ccall "getEntityOri" getEntityOriFFI :: Ptr () -> ERefFFI -> ERefFFI -> IO (Ptr (Quaternion CFloat))
type DoEntitiesCallback = ERefFFI->IO ()
foreign import ccall "wrapper" mkDoEntitiesCallback :: DoEntitiesCallback -> IO (FunPtr DoEntitiesCallback)
foreign import ccall "doEntities" doEntitiesFFI :: Ptr () -> FunPtr DoEntitiesCallback -> IO ()

newWorld :: IO World
newWorld = World <$> (acceptPtr =<< newWorldFFI)
updateWorld :: World -> IO ()
updateWorld (World world) = withForeignPtr world updateWorldFFI

newEntity :: World -> EntityType -> Point V3 Int32 -> IO EntityRef
newEntity (World world) entityType (P (V3 x y z)) = 
	withForeignPtr world $ \wld ->
	EntityRef <$> newEntityFFI wld (fromIntegral $ fromEnum entityType) x y z

locatedForceEntity :: World -> EntityRef -> V3 Float -> Point V3 Float -> IO ()
locatedForceEntity (World world) (EntityRef entityRef) (V3 fx fy fz) (P (V3 px py pz)) =
	withForeignPtr world $ \wld ->
	locatedForceEntityFFI wld entityRef (CFloat fx) (CFloat fy) (CFloat fz) (CFloat px) (CFloat py) (CFloat pz)

_moveEntity :: World -> EntityRef -> V3 Float -> IO ()
_moveEntity (World world) (EntityRef entityRef) (V3 x y z) =
	withForeignPtr world $ \wld ->
	moveEntityFFI wld entityRef (CFloat x) (CFloat y) (CFloat z)
_rotateEntity :: World -> EntityRef -> Quaternion Float -> IO ()
_rotateEntity (World world) (EntityRef entityRef) (Quaternion w (V3 x y z)) =
	withForeignPtr world $ \wld ->
	rotateEntityFFI wld entityRef (CFloat w) (CFloat x) (CFloat y) (CFloat z)

forceEntity :: World -> EntityRef -> V3 Float -> IO ()
forceEntity (World world) (EntityRef entityRef) (V3 x y z) = 
	withForeignPtr world $ \wld ->
	forceEntityFFI wld entityRef (CFloat x) (CFloat y) (CFloat z)
angularForceEntity :: World -> EntityRef -> V3 Float -> IO ()
angularForceEntity (World world) (EntityRef entityRef) (V3 x y z) = 
	withForeignPtr world $ \wld ->
	angularForceEntityFFI wld entityRef (CFloat x) (CFloat y) (CFloat z)
angularXForceEntity :: World -> EntityRef -> Float -> IO ()
angularXForceEntity world entityRef a = angularForceEntity world entityRef (V3 a 0 0)
angularYForceEntity :: World -> EntityRef -> Float -> IO ()
angularYForceEntity world entityRef a = angularForceEntity world entityRef (V3 0 a 0)
angularZForceEntity :: World -> EntityRef -> Float -> IO ()
angularZForceEntity world entityRef a = angularForceEntity world entityRef (V3 0 0 a)
angularEulerForceEntity :: World -> EntityRef -> V3 Float -> IO ()
angularEulerForceEntity (World world) (EntityRef entityRef) (V3 x y z) = 
	withForeignPtr world  $ \wld ->
	angularEulerForceEntityFFI wld entityRef (CFloat x) (CFloat y) (CFloat z)

getEntityPos :: World -> EntityRef -> EntityRef -> IO (Point V3 Float)
getEntityPos (World world) (EntityRef rootEntityRef) (EntityRef entityRef) =
	withForeignPtr world $ \wld ->
	(P . fmap (\(CFloat f)->f) <$>) $
	flip usePtr peek =<< getEntityPosFFI wld rootEntityRef entityRef
getEntityOri :: World -> EntityRef -> EntityRef -> IO (Quaternion Float)
getEntityOri (World world) (EntityRef rootEntityRef) (EntityRef entityRef) =
	withForeignPtr world $ \wld ->
	(fmap (\(CFloat f)->f) <$>) $
	flip usePtr peek =<< getEntityOriFFI wld rootEntityRef entityRef

doEntities :: World -> (EntityRef->IO()) -> IO ()
doEntities (World world) callback = 
	withForeignPtr world $ \wld ->
	mkDoEntitiesCallback (\er->callback $ EntityRef er) >>=
	\fp -> doEntitiesFFI wld fp >> freeHaskellFunPtr fp

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

