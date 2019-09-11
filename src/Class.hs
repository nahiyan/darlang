module Class where

import Data.Binary.Put (Put, putWord8, putWord16be, putStringUtf8, putWord32be)
import Data.Word (Word16)
import Data.List as List
import Data.List.Split (splitOn)

import Types (Class(..), Model(..))
import Helper (intToWord16, intToWord32, word16ToWord8)
import ConstantPool (classRef, addCPItems)
import Debug.Trace (trace)


process :: Class -> Model -> ( Model, [ Word16 ] )
process class_ model =
    let
        ( constantPoolNew, indexes ) =
            addCPItems
                (Types.constantPool model)
                (classRef (Types.className class_)
                    ++ classRef "java/lang/Object"
                )

        indexes2 =
            if List.length indexes >= 4 then
                [ indexes !! 1, indexes !! 3 ]
            else
                []

        modelNew =
            model
                { constantPool = constantPoolNew }
    in
    ( modelNew, indexes2 )