{-# LANGUAGE DeriveDataTypeable #-}

import Prelude hiding (catch)
import Control.Exception
import Data.Typeable
import Data.Time.Clock
import Control.Concurrent

data TimedOut = TimedOut UTCTime deriving (Eq, Show, Typeable)
instance Exception TimedOut

-- Execute IO action, or abort after # of µsec

timeout :: Int -> IO a -> IO (Maybe a)
timeout usec action = do
  -- Create unique exception val (for nested timeouts):
  expired <- fmap TimedOut getCurrentTime

  ptid <- myThreadId
  let child = do threadDelay usec
                 throwTo ptid expired
      parent = do ctid <- forkIO child
                  result <- action
                  killThread ctid
                  return $ Just result
  catchJust (\e -> if e == expired then Just e else Nothing) 
            parent
            (\_ -> return Nothing)
