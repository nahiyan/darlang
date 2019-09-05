module ClassFile where

import Data.ByteString.Char8 as BSC
import Data.ByteString.Lazy as BS
import Data.Binary.Put (Put, runPut, putWord8, putWord16be, putStringUtf8)
import Data.Word (Word16)
import Data.List as List

import Helper (intToWord16)
import ConstantPool as CP
import Types (Model(..), Class(..), Method(..))
import Method (sizeBC, bytecode)


bytecode :: Types.Class -> BS.ByteString
bytecode class_ =
    let
        initModel =
            Model
                { constantPool = [] }

        interfacesCount = 0

        fieldsCount = 0

        methodsCount =
            List.length $ Types.methods class_

        attributesCount = 0

        processedMethodsModel =
            Method.process
                (Types.methods class_)
                initModel

        processedConstantPoolModel =
            CP.process
                processedMethods

        constantPoolRelatedBC =
            Types.constantPoolBC processedConstantPoolModel

        interfacesRelatedBC =
            if interfacesCount == 0 then
                interfacesCountBC
            else
                interfacesCountBC
                    >> interfacesBC

        fieldsRelatedBC =
            if fieldsCount == 0 then
                fieldsCountBC
            else
                fieldsCountBC
                    >> fieldsBC

        methodsRelatedBC =
            Types.methodsBC processedMethodsModel

        attributesRelatedBC =
            if attributesCount == 0 then
                attributesCountBC
            else
                attributesCountBC
                    >> attributesBC

        magicNumberBC = do
            putWord8 202    -- ca
            putWord8 254    -- fe
            putWord8 186    -- ba
            putWord8 190    -- be

        minorVersionBC = do
            putWord16be 0

        majorVersionBC = do
            putWord16be 52

        accessFlagsBC = do
            putWord16be 33

        thisClassBC =
            putWord16be 1

        superClassBC =
            putWord16be 3

        interfacesCountBC = do
            putWord16be 0

        interfacesBC =
            return ()

        fieldsCountBC = do
            putWord16be 0

        fieldsBC =
            return ()

        attributesCountBC = do
            putWord16be 0

        attributesBC =
            return ()

        serialization =
            magicNumberBC
                >> minorVersionBC
                >> majorVersionBC
                >> constantPoolRelatedBC
                >> accessFlagsBC
                >> thisClassBC
                >> superClassBC
                >> interfacesRelatedBC
                >> fieldsRelatedBC
                >> methodsRelatedBC
                >> attributesRelatedBC
    in
    runPut serialization


writeClassFile :: Types.Class -> IO ()
writeClassFile class_ =
    let
        contents =
            ClassFile.bytecode class_

        className_ =
            Types.className class_
    in
    BS.writeFile className_ contents
        >> Prelude.putStrLn "File Written"