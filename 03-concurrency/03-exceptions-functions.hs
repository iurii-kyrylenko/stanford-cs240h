{-# LANGUAGE DeriveDataTypeable #-}

import Prelude hiding (catch)
import Control.Exception
import Data.Typeable

data MyError = MyError String deriving (Show, Typeable)
instance Exception MyError

t1 = do
  putStrLn "type 1 for exception..."
  x <- getLine
  y <- if x == "1"
    then
      throwIO $ MyError $ x ++ ": exception from IO"
    else do
      putStrLn "type 2 for exception..."
      getLine
  if y == "2"
    then throwIO $ MyError $ y ++ ": exception from IO"
    else return $ x ++ y

-- t2 :: IO (Either MyError String)
t2 :: IO (Either SomeException String)
t2 = try t1

t3 = finally t1 (putStrLn "-- finalization --")

t4 = onException t1 (putStrLn "-- finalization on exception --")

-- catchJust #1 --

t5 = catchJust p t1 (\(MyError msg) -> return msg)
  where
    p (MyError "1: exception from IO") = Just (MyError "1111")
    p (MyError "2: exception from IO") = Just (MyError "2222")

-- catchJust #2 --

data YourError = YourError deriving (Show, Typeable)
instance Exception YourError

t6 = do
  putStrLn "type 1 for exception..."
  x <- getLine
  y <- if x == "1"
    then
      throwIO $ MyError $ x ++ ": exception from IO"
    else do
      putStrLn "type 2 for exception..."
      getLine
  if y == "2"
    then throwIO YourError
    else return $ x ++ y

t7 = catchJust p t6 (\(MyError msg) -> return msg)
  where -- MyError actually won't be caught
    p YourError = Just (MyError "This is your error, not mine !")

