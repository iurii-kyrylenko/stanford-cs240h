module Utf16 (encodeChar) where

import Data.Word (Word16)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

encodeChar :: Char -> [Word16]
encodeChar x
  | w < 0x10000 = [fromIntegral w]
  | otherwise = [fromIntegral hi, fromIntegral lo]
  where
    w = ord x
    hi = ((w - 0x10000) `shiftR` 10) + 0xD800
    lo = (w .&. 0x03FF) + 0xDC00
