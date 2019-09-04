module Components where

import Data.ByteString.Char8 as BSC
import Data.ByteString.Lazy as BS
import Data.Binary.Put (Put, runPut, putWord8, putWord16be, putStringUtf8)
import Data.Word (Word16)
import Data.List as List

import Helper (intToWord16)
import ConstantPool (constantPoolSize, generateConstantPoolWords)
import Types (Component)


magicNumberWords :: Put
magicNumberWords = do
    putWord8 202    -- ca
    putWord8 254    -- fe
    putWord8 186    -- ba
    putWord8 190    -- be


minorVersionWords :: Put
minorVersionWords = do
    putWord8 0      -- 00
    putWord8 0      -- 00


majorVersionWords :: Put
majorVersionWords = do
    putWord8 0      -- 00
    putWord8 52     -- 34


constantPoolSizeWords :: Word16 -> Put
constantPoolSizeWords =
    putWord16be


constantPoolWords :: [ Component ] -> Put
constantPoolWords components =
    generateConstantPoolWords 1 components [] components


accessFlagsWords :: Put
accessFlagsWords = do
    putWord8 0      -- 00
    putWord8 33     -- 21


thisClassWords :: Put
thisClassWords =
    putWord16be 1


superClassWords :: Put
superClassWords =
    putWord16be 3


interfacesCountWords :: Put
interfacesCountWords = do
    putWord8 0      -- 00
    putWord8 0      -- 00


interfacesWords :: Put
interfacesWords =
    return ()


fieldsCountWords :: Put
fieldsCountWords = do
    putWord8 0      -- 00
    putWord8 0      -- 00


fieldsWords :: Put
fieldsWords =
    return ()


methodsCountWords :: Put
methodsCountWords = do
    putWord8 0      -- 00
    putWord8 0      -- 00


methodsWords :: Put
methodsWords =
    return ()


attributesCountWords :: Put
attributesCountWords = do
    putWord8 0      -- 00
    putWord8 0      -- 00


attributesWords :: Put
attributesWords =
    return ()


generateByteCode :: [ Component ] -> BS.ByteString
generateByteCode components =
    let
        constantPoolSize' = constantPoolSize components
        interfacesCount = 0
        fieldsCount = 0
        methodsCount = 0
        attributesCount = 0

        constantPoolRelatedWords =
            if constantPoolSize' == 0 then
                constantPoolSizeWords constantPoolSize'
            else
                constantPoolSizeWords constantPoolSize'
                    >> constantPoolWords components

        interfacesRelatedWords =
            if interfacesCount == 0 then
                interfacesCountWords
            else
                interfacesCountWords
                    >> interfacesWords

        fieldsRelatedWords =
            if fieldsCount == 0 then
                fieldsCountWords
            else
                fieldsCountWords
                    >> fieldsWords

        methodsRelatedWords =
            if methodsCount == 0 then
                methodsCountWords
            else
                methodsCountWords
                    >> methodsWords

        attributesRelatedWords =
            if attributesCount == 0 then
                attributesCountWords
            else
                attributesCountWords
                    >> attributesWords

        serialization =
            magicNumberWords
                >> minorVersionWords
                >> majorVersionWords
                >> constantPoolRelatedWords
                >> accessFlagsWords
                >> thisClassWords
                >> superClassWords
                >> interfacesRelatedWords
                >> fieldsRelatedWords
                >> methodsRelatedWords
                >> attributesRelatedWords
    in
    runPut serialization