module Helper where

import           Data.Word ( Word16
                           , Word32
                           , Word8 )

intToWord16 :: Int
            -> Word16
intToWord16 a =
    fromIntegral (a :: Int) :: Word16

intToWord32 :: Int
            -> Word32
intToWord32 a =
    fromIntegral (a :: Int) :: Word32

word16ToWord8 :: Word16
              -> Word8
word16ToWord8 a =
    fromIntegral (a :: Word16) :: Word8
