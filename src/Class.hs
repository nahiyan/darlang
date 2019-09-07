module Class where

import Data.Binary.Put (Put, putWord8, putWord16be, putStringUtf8, putWord32be)
import Data.Word (Word16)
import Data.List as List
import Data.List.Split (splitOn)

import Types (Class(..), Model(..))
import Helper (intToWord16, intToWord32, word16ToWord8)
import ConstantPool (addCPItem, addClassRef)


process :: Class -> Model -> Model
process class_ model =
    let
        ( constantPoolNew, _ ) =
            addCPItem
                (Types.constantPool model)
                [ ( "class_ref", "2" )
                , ( "utf8", Types.className class_ )
                , ( "class_ref", "4" )
                , ( "utf8", "java/lang/Object" )
                ]
    in
    model
        { constantPool = constantPoolNew
        }