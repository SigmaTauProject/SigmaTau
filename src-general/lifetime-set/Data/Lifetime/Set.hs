module Data.Lifetime.Set (LifetimeSet,newLifetimeSet,add,withAll,withAll_) where

import Data.IORef
import Data.Maybe (catMaybes)

import Data.Lifetime

data LifetimeSet a = LifetimeSet (IORef ([Lifetime a]))

newLifetimeSet :: IO (LifetimeSet a)
newLifetimeSet = LifetimeSet <$> newIORef []

add :: LifetimeSet a -> Lifetime a -> IO ()
add (LifetimeSet ltsRef) l = do
	lts <- readIORef ltsRef
	writeIORef ltsRef (l:lts)

withAll :: LifetimeSet a -> (a -> IO b) -> IO ([b])
withAll (LifetimeSet ltsRef) f = do
	lts <- readIORef ltsRef
	run <- catMaybes <$> (sequence $ (\l->(,) l <$$> with l f) <$> lts)
	writeIORef ltsRef (fst <$> run)
	return $ snd <$> run

withAll_ :: LifetimeSet a -> (a -> IO ()) -> IO ()
withAll_ ls f = withAll ls f >> return ()






(<$$>) :: (Functor f0, Functor f1) =>
          (a -> b)
       -> f1 (f0 a)
       -> f1 (f0 b)
(<$$>) = fmap . fmap
