import Control.Monad
import Test.QuickCheck

data Point a = Point a a
               deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Point a) where
  arbitrary = liftM2 Point arbitrary arbitrary
  shrink (Point x y) = liftM2 Point (shrink x) (shrink y)

g :: Gen (Point Int)
g = arbitrary
s = sample g

ts :: [Point Int]
ts = shrink $ Point 3 4
