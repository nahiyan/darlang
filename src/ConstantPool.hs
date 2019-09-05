module ConstantPool where

import Data.Binary.Put (Put, runPut, putWord8, putWord16be, putStringUtf8)
import Data.Word (Word16)
import Data.List as List
import Data.List.Split (splitOn)
import Control.Monad (when)

import Types
import Helper (intToWord16)


sizeBC :: Word16 -> Put
sizeBC =
    putWord16be


size :: [ Component ] -> Word16
size constantPool =
    List.foldl
        (+)
        1
        (List.map size' constantPool)


size' :: Component -> Word16
size' ( a, _ ) =
    case a of
        "class_ref" ->
            2

        "string_ref" ->
            2

        "field_ref" ->
            4

        "method_ref" ->
            4

        "utf8" ->
            1

        "method" ->
            3

        _ ->
            0


model :: [ Component ] -> CPModel
model components =
    let
        initModel =
            CPModel
                { bytecode = return ()
                , methodsInfo = []
                , components = components
                }
    in
    model' 1 components initModel


model' :: Word16 -> [ Component ] -> CPModel -> CPModel
model' index items model =
    if null items then
        model
    else
        let
            currentItem =
                List.head items

            indexNew =
                index + size' currentItem

            itemsNew =
                List.tail items

            modelNew =
                model'' index currentItem model
        in
        model' indexNew itemsNew modelNew


model'' :: Word16 -> Component -> CPModel -> CPModel
model'' index ( a, b ) model =
    let
        oldBytecode =
            Types.bytecode model

        oldMethodsInfo =
            Types.methodsInfo model
    in
    case a of
        "class_ref" ->
            model
                { bytecode = oldBytecode >> classRefBC index b }

        "string_ref" ->
            model
                { bytecode = oldBytecode >> stringRefBC index b }

        "field_ref" ->
            model
                { bytecode = oldBytecode >> fieldRefBC index b (components model) }

        "method_ref" ->
            model
                { bytecode = oldBytecode >> methodRefBC index b (components model) }

        "utf8" ->
            model
                { bytecode = oldBytecode >> utf8BC b }

        "method" ->
            let
                result =
                    methodBC index b
            in
            model
                { bytecode = oldBytecode >> fst result
                , methodsInfo = oldMethodsInfo ++ snd result
                }

        _ ->
            model


itemsBefore :: [ Component ] -> Int -> Word16
itemsBefore constantPool index =
    let
        constantPoolTrimmed =
            List.take (index - 1) constantPool
    in
    List.sum $ List.map size' constantPoolTrimmed


classRefBC :: Word16 -> String -> Put
classRefBC index name = do
    putWord8 7
    putWord16be $ index + 1

    putWord8 1
    putWord16be $ intToWord16 $ Prelude.length name

    putStringUtf8 name


utf8BC :: String -> Put
utf8BC contents = do
    putWord8 1
    putWord16be $ intToWord16 $ Prelude.length contents
    putStringUtf8 contents


stringRefBC :: Word16 -> String -> Put
stringRefBC index contents = do
    putWord8 8
    putWord16be $ index + 1
    utf8BC contents


fieldOrMethodRefBC :: Word16 -> String -> [ Component ] -> String -> Put
fieldOrMethodRefBC index contents constantPool refType =
    let
        segments =
            splitOn ":" contents
    in
    when (List.length segments == 3) $
        let
            parentClassCPIndex =
                read (List.head segments) :: Int

            parentClassIndex =
                itemsBefore constantPool parentClassCPIndex + 1

            name =
                segments !! 1

            type_ =
                segments !! 2
        in
        do
            case refType of
                "field" ->
                    putWord8 9

                _ ->
                    putWord8 10

            putWord16be parentClassIndex
            putWord16be $ index + 1

            nameAndTypeBC (index + 1) (name ++ ":" ++ type_)


fieldRefBC :: Word16 -> String -> [ Component ] -> Put
fieldRefBC index contents constantPool =
    fieldOrMethodRefBC index contents constantPool "field"


methodRefBC :: Word16 -> String -> [ Component ] -> Put
methodRefBC index contents constantPool =
    fieldOrMethodRefBC index contents constantPool "method"


nameAndTypeBC :: Word16 -> String -> Put
nameAndTypeBC index contents =
    let
        segments =
            splitOn ":" contents
    in
    when (List.length segments == 2) $
        let
            name =
                List.head segments

            type_ =
                segments !! 1
        in
        do
            putWord8 12
            putWord16be $ index + 1
            putWord16be $ index + 2
            utf8BC name
            utf8BC type_


-- methodBC :: Word16 -> String -> ( Put, [ MethodInfo ] )
-- methodBC index contents =
--     let
--         segments =
--             splitOn ":" contents
--     in
--     if List.length segments == 2 then
--         let
--             name =
--                 List.head segments

--             type_ =
--                 segments !! 1

--             bytecode_ = do
--                 utf8BC name
--                 utf8BC type_
--                 utf8BC "Code"

--             methodInfo =
--                 index
--         in
--         ( bytecode_, [ methodInfo ] )

--     else
--         ( return (), [] )