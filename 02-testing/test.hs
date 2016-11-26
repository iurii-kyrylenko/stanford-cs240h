import Test.QuickCheck
import Data.Char
import Data.Word (Word16)
import Utf16

prop_encodeOne :: Char -> Bool
prop_encodeOne c = length (encodeChar c) == 1

t1 = quickCheck prop_encodeOne

newtype BigChar = BigChar Char deriving (Show, Ord, Eq)

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

-- Generating directly --
prop_encodeOne' = do
  BigChar c <- (BigChar . chr) `fmap` choose (0x0000, 0xFFFF)
  return $ length (encodeChar c) == 1

-- Generating filtering --
prop_encodeOne'' = do
  BigChar c <- arbitrary `suchThat` (< BigChar '\x10000')
  return $ length (encodeChar c) == 1

-- Generating filtering (consice) --
prop_encodeOne''' (BigChar c) = c < '\x10000' ==> length (encodeChar c) == 1
