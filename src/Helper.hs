module Helper where

import Data.Word (Word16)

intToWord16 :: Int -> Word16
intToWord16 a =
    fromIntegral (a :: Int) :: Word16