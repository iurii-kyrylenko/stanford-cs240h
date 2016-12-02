import Control.Concurrent
import Control.Exception
import Control.Monad

pingpong :: Bool -> Int -> IO ()
pingpong v n = do
  mvp <- newEmptyMVar
  mvc <- newEmptyMVar
  let parent n | n >= 0 = do when v $ putStr $ " " ++ show n
                             putMVar mvc n
                             (takeMVar mvp) >>= parent
               | otherwise = return ()
      child = do n <- takeMVar mvc
                 putMVar mvp (n-1)
                 child
  tid <- forkIO child
  (parent n) `finally` (killThread tid)
  when v $ putStrLn ""
