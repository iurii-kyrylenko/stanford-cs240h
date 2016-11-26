import Control.Monad
import Test.QuickCheck

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving (Show)

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = sized tree

tree :: (Arbitrary a) => Int -> Gen (Tree a)
tree 0 = liftM Leaf arbitrary
tree n = oneof [
    liftM Leaf arbitrary
  , liftM2 Node subtree subtree
  ] where subtree = tree $ n `div` 2

g :: Gen (Tree Int)
g = arbitrary

s = sample g
