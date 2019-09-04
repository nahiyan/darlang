module ConstantPool where

import Data.Binary.Put (Put, runPut, putWord8, putWord16be, putStringUtf8)
import Data.Word (Word16)
import Data.List as List
import Data.List.Split (splitOn)

import Types (Component)
import Helper (intToWord16)


constantPoolSize' :: Component -> Word16
constantPoolSize' ( a, _ ) =
    case a of
        "class" ->
            2

        "string" ->
            2

        "field" ->
            4

        "method" ->
            4

        _ ->
            0


constantPoolSize :: [ Component ] -> Word16
constantPoolSize constantPool =
    List.foldl
        (+)
        1
        (List.map constantPoolSize' constantPool)


constantPoolItemsBefore :: [ Component ] -> Int -> Word16
constantPoolItemsBefore constantPool index =
    let
        constantPoolTrimmed =
            List.take (index - 1) constantPool
    in
    List.sum $ List.map constantPoolSize' constantPoolTrimmed


generateConstantPoolWords' :: Word16 -> Component -> [ Component ] -> Put
generateConstantPoolWords' index ( a, b ) constantPool =
    case a of
        "class" ->
            classNameWords index b

        "string" ->
            stringWords index b

        "field" ->
            fieldWords index b constantPool

        "method" ->
            methodWords index b constantPool

        _ ->
            putWord8 0


generateConstantPoolWords :: Word16 -> [ Component ] -> [ Put ] -> [ Component ] -> Put
generateConstantPoolWords index items words constantPool =
    if null items then
        List.foldl1 (>>) words
    else
        let
            currentItem =
                List.head items

            counterNew =
                index + constantPoolSize' currentItem

            itemsNew =
                List.tail items

            wordsNew =
                words
                    ++ [ generateConstantPoolWords' index currentItem constantPool ]
        in
        generateConstantPoolWords counterNew itemsNew wordsNew constantPool


classNameWords :: Word16 -> String -> Put
classNameWords index name = do
    putWord8 7
    putWord16be $ index + 1

    putWord8 1
    putWord16be $ intToWord16 $ Prelude.length name

    putStringUtf8 name


utf8Words :: String -> Put
utf8Words contents = do
    putWord8 1
    putWord16be $ intToWord16 $ Prelude.length contents
    putStringUtf8 contents


stringWords :: Word16 -> String -> Put
stringWords index contents = do
    putWord8 8
    putWord16be $ index + 1
    utf8Words contents


fieldWords :: Word16 -> String -> [ Component ] -> Put
fieldWords index contents constantPool =
    let
        segments =
            splitOn ":" contents
    in
    if List.length segments == 3 then
        let
            parentClassCPIndex =
                read (List.head segments) :: Int

            parentClassIndex =
                (constantPoolItemsBefore constantPool parentClassCPIndex) + 1

            name =
                segments !! 1

            type_ =
                segments !! 2
        in
        do
            putWord8 9
            putWord16be parentClassIndex
            putWord16be $ index + 1

            nameAndTypeWords (index + 1) (name ++ ":" ++ type_)
    else
        return ()


methodWords :: Word16 -> String -> [ Component ] -> Put
methodWords index contents constantPool =
    let
        segments =
            splitOn ":" contents
    in
    if List.length segments == 3 then
        let
            parentClassCPIndex =
                read (List.head segments) :: Int

            parentClassIndex =
                (constantPoolItemsBefore constantPool parentClassCPIndex) + 1

            name =
                segments !! 1

            type_ =
                segments !! 2
        in
        do
            putWord8 10
            putWord16be parentClassIndex
            putWord16be $ index + 1

            nameAndTypeWords (index + 1) (name ++ ":" ++ type_)
    else
        return ()


nameAndTypeWords :: Word16 -> String -> Put
nameAndTypeWords index contents =
    let
        segments =
            splitOn ":" contents
    in
    if List.length segments == 2 then
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
            utf8Words name
            utf8Words type_
    else
        return ()