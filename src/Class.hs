module Class where

import Data.Binary.Put (Put, putWord8, putWord16be, putStringUtf8, putWord32be)
import Data.Word (Word16)
import Data.List as List
import Data.List.Split (splitOn)

import Types (Class(..), Model(..))
import Helper (intToWord16, intToWord32, word16ToWord8)
import ConstantPool (addClassRef, combineCPAndIndexes)


process :: Class -> Model -> ( Model, [ Word16 ] )
process class_ model =
    let
        -- ( constantPoolNew, indexes ) =
        --     addClassRef
        --         (Types.className class_)
        --         (Types.constantPool model)

        -- ( constantPoolNew2, indexes2 ) =
        --     addClassRef
        --         "java/lang/Object"
        --         constantPoolNew

        ( constantPoolNew, indexes ) =
            combineCPAndIndexes
                [ addClassRef
                    (Types.className class_)
                    (Types.constantPool model)
                , addClassRef
                    "java/lang/Object"
                    (Types.constantPool model)
                ]

        -- indexes3 =
        --     [ List.head indexes, List.head indexes2 ]

        indexes2 =
            [ List.head indexes, indexes !! 2 ]

        modelNew =
            model
                { constantPool = constantPoolNew }
    in
    ( modelNew, [ 1, 2 ] )