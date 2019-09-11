module ConstantPool where

import Data.Binary.Put (Put, runPut, putWord8, putWord16be, putStringUtf8)
import Data.Word (Word16)
import Data.List as List
import Data.List.Split (splitOn)
import Control.Monad (when)
import Debug.Trace (trace)

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

process' :: Model -> ConstantPool -> Model
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
                List.tail constantPool
        in
        process' modelNew constantPoolNew


process'' :: Model -> CPItem -> Model
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


addCPItem :: ConstantPool -> CPItem -> ( ConstantPool, Word16 )
addCPItem constantPool item =
    let
        constantPoolNew =
            constantPool ++ [ item ]

        index =
            intToWord16 $ List.length constantPoolNew
    in
    ( constantPoolNew, index )


addCPItems' :: ConstantPool -> ConstantPool -> [ Word16 ] -> [ Word16 ] -> ( ConstantPool, [ Word16 ] )
addCPItems' constantPool items indexes stack =
    if null items then
        ( constantPool, indexes )
    else
        let
            (currentItem:restOfItems) =
                items

            contents
                | snd currentItem == "%x" && not (null stack) =
                  show $ List.last stack
                | snd currentItem == "%x:%x" && List.length stack >= 2 =
                  let lastTwoItems = List.take 2 $ List.reverse stack
                      a = show $ List.head lastTwoItems
                      b = show $ lastTwoItems !! 1
                    in a ++ ":" ++ b
                | otherwise = snd currentItem

            currentItemNew =
                ( fst currentItem, contents )

            stackNew
                | snd currentItem == "%x" && not (null stack) =
                    List.take (List.length stack - 1) stack
                | snd currentItem == "%x:%x" && List.length stack >= 2 =
                    List.take (List.length stack - 2) stack
                | otherwise = stack

                -- if snd currentItem == "%x" && not (null stack) then
                --     List.take (List.length stack - 1) stack
                -- else if snd currentItem == "%x:%x" && List.length stack >= 2 then
                --     List.take (List.length stack - 2) stack
                -- else
                --     stack

            ( constantPoolNew, index ) =
                addCPItem constantPool currentItemNew

            stackNew2 =
                stackNew ++ [ index ]

            indexesNew =
                indexes ++ [ index ]
        in
        addCPItems' constantPoolNew restOfItems indexesNew stackNew2


addCPItems :: ConstantPool -> ConstantPool -> ( ConstantPool, [ Word16 ] )
addCPItems constantPool newItems =
    addCPItems' constantPool newItems [] []


classRef :: String -> ConstantPool
classRef name =
    [ ( "utf8", name )
    , ( "class_ref", "%x" )
    ]


stringRef :: String -> ConstantPool
stringRef name =
    [ ( "utf8", name )
    , ( "string_ref", "%x" )
    ]


fieldOrMethodRef :: [ String ] -> String -> ConstantPool
fieldOrMethodRef segments refType =
    if List.length segments == 3 then
        let
            classRef_ =
                classRef (List.head segments)

            nameAndType_ =
                nameAndType
                    (segments !! 1)
                    (segments !! 2)

            fieldOrMethodRef_ =
                [ ( refType ++ "_ref", "%x:%x" ) ]
        in
        nameAndType_
            ++ classRef_
            ++ fieldOrMethodRef_
    else
        []


fieldRef :: [ String ] -> ConstantPool
fieldRef segments =
    fieldOrMethodRef segments "field"


methodRef :: [ String ] -> ConstantPool
methodRef segments =
    fieldOrMethodRef segments "method"


nameAndType :: String -> String -> ConstantPool
nameAndType name type_ =
    [ ( "utf8", type_)
    , ( "utf8", name )
    , ( "name_and_type", "%x:%x" )
    ]


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
