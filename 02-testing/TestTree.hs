import Control.Monad
import Test.QuickCheck

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving (Show)

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = oneof [
                      liftM  Leaf arbitrary
                    , liftM2 Node arbitrary arbitrary
                    ]

g :: Gen (Tree Int)
g = arbitrary

ts = sample g
