-- Pattern match errors

t1 :: IO String
t1 = do
  putStrLn "Any input is OK"
  x <- getLine
  return x

t2 :: IO String
t2 = do
  putStrLn "Only 42 is OK. All else raises exception"
  "42" <- getLine
  return "42"

-- Any int param is OK
t3 :: Int -> Maybe Int
t3 x = do
  a <- return x
  return a

-- Only 42 is OK. All else is nothing
t4 :: Int -> Maybe Int
t4 x = do
  42 <- return x
  return 42
