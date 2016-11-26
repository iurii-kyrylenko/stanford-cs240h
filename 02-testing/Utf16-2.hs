{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

import Data.Bits ((.&.), shiftR, shiftL)
import Data.Char (chr, ord)
import Data.Word (Word16)
import System.Random (Random)
import Test.QuickCheck hiding ((.&.))

encodeChar :: Char -> [Word16]
encodeChar x
  | w < 0x10000 = [fromIntegral w]
  | otherwise   = [fromIntegral a, fromIntegral b]
  where w = ord x
        a = ((w - 0x10000) `shiftR` 10) + 0xD800
        b = (w .&. 0x3FF) + 0xDC00

encodeUtf16 :: [Char] -> [Word16]
encodeUtf16 = concatMap encodeChar

newtype BigChar = Big Char
                deriving (Eq, Ord, Show, Random)

instance Arbitrary BigChar where
    arbitrary = choose (Big '\0',Big '\x10FFFF')
    shrink (Big c) = map (Big . chr) . shrink . ord $ c

decodeUtf16 :: [Word16] -> [Char]
decodeUtf16 []  = []
decodeUtf16 [w] = [decodeChar w]
decodeUtf16 (w1:w2:ws)
  | w1 < 0xD800 = decodeChar w1 : decodeUtf16 (w2 : ws)
  | otherwise   = (decodeBigChar w1 w2) : decodeUtf16 ws

decodeChar :: Word16 -> Char
decodeChar = chr . fromIntegral

decodeBigChar :: Word16 -> Word16 -> Char
decodeBigChar h l = chr (a + b)
  where a = (fromIntegral l) - 0xDC00
        b = ((fromIntegral h) - 0xD800) `shiftL` 10 + 0x10000

--decodeUtf16 . encodeUtf16 = id
prop_combine :: String -> Bool
prop_combine s = (decodeUtf16 . encodeUtf16) s == s

-- fails for codepoints from U+D800 to U+DFFF
prop_combine' :: [BigChar] -> Bool
prop_combine' s = (decodeUtf16 . encodeUtf16 . toChars) s == toChars s
  where toChars = fmap $ \(Big c) -> c
