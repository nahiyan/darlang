module Method where

import Types (Model(..))

import Data.Binary.Put (Put, putWord8, putWord16be, putStringUtf8)
import Data.Word (Word16)
import Data.List as List


sizeBC :: Put
sizeBC =
    putWord16be 0


process :: Model -> Put
process model =
    -- methodsWords' 1 components [] components
    return ()


-- methodsWords' :: Word16 -> [ Component ] -> [ Put ] -> [ Component ] -> Put
-- methodsWords' index items words constantPool =
--     if null items then
--         List.foldl1 (>>) words
--     else
--         let
--             currentItem =
--                 List.head items

--             counterNew =
--                 index + constantPoolSize' currentItem

--             itemsNew =
--                 List.tail items

--             wordsNew =
--                 words
--                     ++ [ methodWords index currentItem constantPool ]
--         in
--         methodWords' counterNew itemsNew wordsNew constantPool
    

bytecode' :: String -> Word16 -> Word16 -> Put
bytecode' access_flags name_index type_index =
    let
        access_flags_words =
            case access_flags of
                "public static" ->
                    9

                _ ->
                    2

    in
    do
        putWord16be access_flags_words
        putWord16be name_index
        putWord16be type_index
        putWord16be 0
