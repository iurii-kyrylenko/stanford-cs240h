{-# LANGUAGE DeriveDataTypeable #-}

import Prelude hiding (catch)
import Control.Exception
import Data.Typeable
import Control.DeepSeq (deepseq)


r1 = let (x, y) = (undefined, 42) in x
r2 = let (x, y) = (42, undefined) in x

-- nonstrict function: doesn't evaluate b
fst' :: (a, b) -> a
fst' (a, _) = a

-- nonstrict function: forces to evaluate b
fst'' :: (a, b) -> a
fst'' (a, b) = b `seq` a

pureCatcher :: a -> IO (Maybe a)
pureCatcher a = (a `seq` return $ Just a) `catch` \(SomeException _) -> return Nothing

r3 = pureCatcher $ fst'  (42, undefined) -- Just 42
r4 = pureCatcher $ fst'' (42, undefined) -- Nothing

r5 = pureCatcher (undefined :: [Int]) -- Nothing
-- 'catch' only catches exceptions when a first level of thunk (cons) is evaluated
r6 = pureCatcher [1, 2, 3, undefined] -- Just [1,2,3,*** Exception: Prelude.undefined

-- evaluates every element of list

-- my solution (https://wiki.haskell.org/Seq)
-- seqList :: [a] -> b -> b
-- seqList xs y = foldr (\x a -> x `seq` a) y xs

seqList :: [a] -> b -> b
seqList [] y = y
seqList (x:xs) y = x `seq` (seqList xs y)

r7 = [1,2,3,undefined]                          -- [1,2,3,*** Exception: Prelude.undefined
r8 = seqList [1,2,3,undefined] ()               -- *** Exception: Prelude.undefined
r9 = pureCatcher $ seqList [1,2,3,undefined] () -- Nothing

-- use deepseq
r10 = pureCatcher $ deepseq ([1,2,3,undefined] :: [Int]) () -- Nothing

