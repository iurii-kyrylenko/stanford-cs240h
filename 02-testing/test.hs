import Test.QuickCheck
import Data.Char
import Data.Word (Word16)
import Utf16

prop_encodeOne :: Char -> Bool
prop_encodeOne c = length (encodeChar c) == 1

t1 = quickCheck prop_encodeOne

newtype BigChar = BigChar Char deriving (Show)

instance Arbitrary BigChar where
  -- arbitrary = (BigChar . chr) `fmap` choose (0x10000, 0x10FFFF)
  -- to use the shrink capability
  arbitrary = (BigChar . chr) `fmap` choose (0x00000, 0x20000)
  shrink (BigChar c) = map BigChar (shrinkChar c)

shrinkChar :: Char -> [Char]
shrinkChar = (fmap chr) . shrink . fromIntegral . ord

prop_encodeTwo :: BigChar -> Bool
prop_encodeTwo (BigChar c) = length (encodeChar c) == 1

t2 = quickCheck prop_encodeTwo

prop_encodeTwo' :: BigChar -> Bool
prop_encodeTwo' (BigChar c) = length (encodeChar c) == 2

t3 = quickCheck prop_encodeTwo'
