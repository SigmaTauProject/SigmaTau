module Data.Lifetime (Lifetime(..),newLifetime,with,with_) where

data Lifetime a = Lifetime (IO (Maybe a))

newLifetime :: IO (Maybe a) -> Lifetime a
newLifetime = Lifetime

with :: Lifetime a -> (a -> IO b) -> IO (Maybe b)
with (Lifetime lt) f = do
	lv <- lt
	doMaybe $ f <$> lv
with_ :: Lifetime a -> (a -> IO ()) -> IO ()
with_ l f = with l f >> return ()

doMaybe :: Maybe (IO a) -> IO (Maybe a)
doMaybe m = maybe (return Nothing) (Just <$>) m


