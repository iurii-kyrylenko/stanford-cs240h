{-# LANGUAGE DeriveDataTypeable #-}

import Prelude hiding (catch)
import Control.Exception
import Data.Typeable

data MyError = MyError String deriving (Show, Typeable)
instance Exception MyError

catcher :: IO a -> IO (Maybe a)
catcher action = fmap Just action `catch` handler
  where
    handler (MyError msg) = do
      putStrLn msg
      return Nothing

    -- handler e@(SomeException _) = do
    --   putStrLn $ show e
    --   return Nothing

    -- handler e = do
    --   putStrLn $ show (e :: MyError)
    --   return Nothing

t1 = catcher getLine
t2 = catcher $ throwIO $ MyError "-- sorry --"

t3 = throw $ MyError "-- [1] from pure code --"
t4 = throw $ MyError "-- [2] from pure code --"
t5 = t3 + t4

t6 = do
  putStrLn "type 1 for exception..."
  x <- getLine
  putStrLn "type 2 for exception..."
  y <- if x == "1"
    then throwIO $ MyError $ x ++ ": exception from IO"
    else getLine
  if y == "2"
    then throwIO $ MyError $ y ++ ": exception from IO"
    else return $ x ++ y
