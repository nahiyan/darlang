module ClassFile where

import           Class                 ( process )

import           ConstantPool          as CP

import           Data.Binary.Put
                 ( Put
                 , putStringUtf8
                 , putWord16be
                 , putWord8
                 , runPut )
import           Data.ByteString.Char8 as BSC
import           Data.ByteString.Lazy  as BS
import           Data.List             as List
import           Data.Word             ( Word16 )

import           Debug.Trace           ( trace )

import           Helper
                 ( intToWord16 )

import           Method                ( process
                                       , sizeBC )

import           Types
                 ( Class(..)
                 , Method(..)
                 , Model(..) )

bytecode :: Types.Class
         -> BS.ByteString
bytecode class_ =
    let initModel =
            Model { constantPool   =
                        []
                  , constantPoolBC =
                        return ()
                  , methodsBC      =
                        return ()
                  }

        -- counters
        interfacesCount =
            0

        fieldsCount =
            0

        methodsCount =
            List.length $ Types.methods class_

        attributesCount =
            0

        -- process class
        ( processedClassModel
            , [ thisClassIndex, superClassIndex ]
            ) =
            Class.process class_ initModel

        -- process methods
        processedMethodsModel =
            Method.process (Types.methods class_)
                           processedClassModel

        -- process constant pool
        processedConstantPoolModel =
            CP.process processedMethodsModel

        constantPoolRelatedBC =
            Types.constantPoolBC processedConstantPoolModel

        interfacesRelatedBC =
            if interfacesCount == 0
            then interfacesCountBC
            else interfacesCountBC
                >> interfacesBC

        fieldsRelatedBC =
            if fieldsCount == 0
            then fieldsCountBC
            else fieldsCountBC
                >> fieldsBC

        methodsRelatedBC =
            Types.methodsBC processedMethodsModel

        attributesRelatedBC =
            if attributesCount == 0
            then attributesCountBC
            else attributesCountBC
                >> attributesBC

        magicNumberBC =
            do
                putWord8 202    -- ca
                putWord8 254    -- fe
                putWord8 186    -- ba
                putWord8 190    -- be

        minorVersionBC =
            putWord16be 0

        majorVersionBC =
            putWord16be 52

        accessFlagsBC =
            putWord16be 33

        thisClassBC =
            putWord16be thisClassIndex

        superClassBC =
            putWord16be superClassIndex

        interfacesCountBC =
            putWord16be 0

        interfacesBC =
            return ()

        fieldsCountBC =
            putWord16be 0

        fieldsBC =
            return ()

        attributesCountBC =
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

writeClassFile :: Types.Class
               -> IO ()
writeClassFile class_ =
    let contents   =
            ClassFile.bytecode class_

        className_ =
            Types.className class_ ++ ".class"
    in
        BS.writeFile className_ contents
        >> Prelude.putStrLn "File Written"
