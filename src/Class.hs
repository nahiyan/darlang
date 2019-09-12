module Class where

import           Data.Binary.Put (Put, putStringUtf8, putWord16be, putWord32be,
                                  putWord8)
import           Data.List       as List
import           Data.List.Split (splitOn)
import           Data.Word       (Word16)

import           ConstantPool    (addCPItems, classRef)
import           Debug.Trace     (trace)
import           Helper          (intToWord16, intToWord32, word16ToWord8)
import           Types           (Class (..), Model (..))


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
