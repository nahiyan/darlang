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


process :: Model -> Model
process model =
    let
        constantPool =
            Types.constantPool model

        modelNew =
            model
                { constantPoolBC =
                    putWord16be
                        (intToWord16
                            (List.length
                                (Types.constantPool model) + 1
                            )
                        )
                }
    in
    process' modelNew constantPool


-- loop through items in the constant pool

process' :: Model -> Types.ConstantPool -> Model
process' model constantPool =
    if null constantPool then
        model
    else
        let
            currentItem =
                List.head constantPool

            modelNew =
                process'' model currentItem

            constantPoolNew =
                List.tail constantPool :: Types.ConstantPool
        in
        process' modelNew constantPoolNew


process'' :: Model -> ( String, String ) -> Model
process'' model ( type_, contents ) =
    let
        bytecode =
            case type_ of
                "utf8" ->
                    utf8BC contents

                "name_and_type" ->
                    nameAndTypeBC contents

                "class_ref" ->
                    classRefBC contents

                "field_ref" ->
                    fieldRefBC contents

                "method_ref" ->
                    methodRefBC contents

                "string_ref" ->
                    stringRefBC contents

                _ ->
                    return ()
    in
    model
        { constantPoolBC =
            Types.constantPoolBC model
                >> bytecode
        }


addCPItem :: Types.ConstantPool -> Types.ConstantPool -> ( Types.ConstantPool, [ Word16 ] )
addCPItem constantPool newItems =
    let
        constantPoolNew =
            constantPool ++ newItems

        indexes =
            List.map
                intToWord16
                [ (List.length constantPool + 1)..(List.length constantPoolNew) ]
    in
    ( constantPoolNew, indexes )


addClassRef :: String -> ConstantPool -> ( ConstantPool, [ Word16 ] )
addClassRef name constantPool =
    let
        ( constantPoolNew, indexes ) =
            addCPItem
                constantPool
                [ ( "utf8", name ) ]

        ( constantPoolNew2, indexes2 ) =
            addCPItem
                constantPoolNew
                [ ( "class_ref", show $ List.head indexes ) ]
    in
    ( constantPoolNew2, indexes2 ++ indexes )


addStringRef :: String -> ConstantPool -> ( ConstantPool, [ Word16 ] )
addStringRef name constantPool =
    let
        ( constantPoolNew, indexes ) =
            addCPItem
                constantPool
                [ ( "utf8", name ) ]

        ( constantPoolNew2, indexes2 ) =
            addCPItem
                constantPoolNew
                [ ( "string_ref", show $ List.head indexes ) ]
    in
    ( constantPoolNew2, indexes2 ++ indexes )


addFieldOrMethodRef :: [ String ] -> ConstantPool -> String -> ( ConstantPool, [ Word16 ] )
addFieldOrMethodRef segments constantPool refType =
    if List.length segments == 3 then
        let
            ( constantPoolNew, indexes ) =
                addCPItem
                    constantPool
                    [ ( "utf8", segments !! 1 )
                    , ( "utf8", segments !! 2 )
                    ]

            ( constantPoolNew2, indexes2 ) =
                addClassRef
                    (List.head segments)
                    constantPoolNew

            ( constantPoolNew3, indexes3 ) =
                addNameAndType
                    (segments !! 1)
                    (segments !! 2)
                    constantPoolNew2

            indexesStringified =
                show (List.head indexes2)
                    ++ ":"
                    ++ show (List.head indexes3)

            ( constantPoolNew4, indexes4 ) =
                addCPItem
                    constantPoolNew3
                    [ ( refType ++ "_ref", indexesStringified ) ]

        in
        ( constantPoolNew4, indexes4 ++ indexes3 ++ indexes2 ++ indexes )
    else
        ( constantPool, [] )


addFieldRef :: [ String ] -> ConstantPool -> ( ConstantPool, [ Word16 ] )
addFieldRef segments constantPool =
    addFieldOrMethodRef segments constantPool "field"


addMethodRef :: [ String ] -> ConstantPool -> ( ConstantPool, [ Word16 ] )
addMethodRef segments constantPool =
    addFieldOrMethodRef segments constantPool "method"


addNameAndType :: String -> String -> ConstantPool -> ( ConstantPool, [ Word16 ] )
addNameAndType name type_ constantPool =
    let
        ( constantPoolNew, indexes ) =
            addCPItem
                constantPool
                [ ( "utf8", name )
                , ( "utf8", type_)
                ]

        indexesStringified =
            show (List.head indexes)
                ++ ":"
                ++ show (indexes !! 1)

        ( constantPoolNew2, indexes2 ) =
            addCPItem
                constantPoolNew
                [ ( "name_and_type", indexesStringified ) ]
    in
    ( constantPoolNew2, indexes2 ++ indexes )


utf8BC :: String -> Put
utf8BC contents = do
    putWord8 1
    putWord16be $ intToWord16 $ Prelude.length contents
    putStringUtf8 contents


classRefBC :: String -> Put
classRefBC contents =
    let
        index =
            read contents :: Word16
    in
    do
        putWord8 7
        putWord16be index


stringRefBC :: String -> Put
stringRefBC contents =
    let
        index =
            read contents :: Word16
    in
    do
        putWord8 8
        putWord16be index


fieldOrMethodRefBC :: String -> String -> Put
fieldOrMethodRefBC contents refType =
    let
        segments =
            splitOn ":" contents
    in
    when (List.length segments == 2) $
        let
            index1 =
                read (List.head segments) :: Word16

            index2 =
                read (segments !! 1) :: Word16
        in
        do
            case refType of
                "field" ->
                    putWord8 9

                _ ->
                    putWord8 10

            putWord16be index1
            putWord16be index2


fieldRefBC :: String -> Put
fieldRefBC contents =
    fieldOrMethodRefBC contents "field"


methodRefBC :: String -> Put
methodRefBC contents =
    fieldOrMethodRefBC contents "method"


nameAndTypeBC :: String -> Put
nameAndTypeBC contents =
    let
        segments =
            splitOn ":" contents

        index1 =
            read (List.head segments) :: Word16

        index2 =
            read (segments !! 1) :: Word16
    in
    when (List.length segments == 2) $
        do
            putWord8 12
            putWord16be index1
            putWord16be index2


combineCPAndIndexes :: [ ( ConstantPool, [ Word16 ] ) ] -> ( ConstantPool, [ Word16 ] )
combineCPAndIndexes (x:xs) =
    List.foldl
        (\a ->
            \b ->
                ( fst a ++ fst b, snd a ++ snd b )
        )
        x
        xs
