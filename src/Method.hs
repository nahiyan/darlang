module Method where

import Data.Binary.Put (Put, putWord8, putWord16be, putStringUtf8, putWord32be)
import Data.Word (Word16)
import Data.List as List
import Data.List.Split (splitOn)

import Types (Model(..), Method(..), Instruction)
import Helper (intToWord16, intToWord32, word16ToWord8)
import ConstantPool (addCPItem, addFieldRef, addMethodRef, addStringRef)


accessFlagsBC :: String -> Put
accessFlagsBC accessType =
    let
        accessFlags =
            case accessType of
                "public static" ->
                    9

                _ ->
                    2
    in
    putWord16be accessFlags


sizeBC :: Put
sizeBC =
    putWord16be 0


process :: [ Method ] -> Model -> Model
process methods model =
    let
        modelNew =
            model
                { methodsBC =
                    putWord16be
                        $ intToWord16 $ List.length methods
                }
    in
    process' methods modelNew


-- Loop through the methods

process' :: [ Method ] -> Model -> Model
process' methods model =
    if null methods then
        model
    else
        let
            currentMethod =
                List.head methods :: Method

            methodsNew =
                List.tail methods :: [ Method ]

            modelNew =
                process'' currentMethod model

        in
        process' methodsNew modelNew


-- Process each method

process'' :: Method -> Model -> Model
process'' method model =
    let
        accessFlags =
            accessFlagsBC $ Types.accessType method

        ( constantPoolNew, indexes ) =
            addCPItem
                (Types.constantPool model)
                [ ( "utf8", Types.methodName method )
                , ( "utf8", Types.descriptor method )
                ]

        methodsBCNew =
            Types.methodsBC model
                >> accessFlags
                >> putWord16be (List.head indexes)
                >> putWord16be (indexes !! 1)
                >> putWord16be 1

        modelNew =
            model
                { methodsBC = methodsBCNew
                , constantPool = constantPoolNew
                }
    in
    addCodeAttribute
        (Types.code method)
        method
        modelNew


addCodeAttribute :: [ Instruction ] -> Method -> Model -> Model
addCodeAttribute instructions method model =
    let
        code_length =
            List.sum (List.map instructionSize instructions)

        attribute_size =
            intToWord32 $ 12 + code_length

        ( constantPoolNew, indexes ) =
            addCPItem
                (Types.constantPool model)
                [ ( "utf8", "Code" ) ]

        modelNew =
            model
                { constantPool = constantPoolNew
                , methodsBC =
                    Types.methodsBC model
                        >> putWord16be (List.head indexes)
                        >> putWord32be attribute_size
                        >> putWord16be (Types.maxStackSize method)
                        >> putWord16be (Types.maxLocalVarSize method)
                        >> putWord32be (intToWord32 code_length)
                }

        modelNew2 =
            addInstructions instructions modelNew
    in
    modelNew2
        { methodsBC =
            Types.methodsBC modelNew2
                >> putWord16be 0
                >> putWord16be 0
        }


-- recurse through the instructions

addInstructions :: [ Instruction ] -> Model -> Model
addInstructions instructions model =
    if null instructions then
        model
    else
        let
            currentInstruction =
                List.head instructions

            instructionsNew =
                List.tail instructions

            modelNew =
                addInstructions' currentInstruction model

        in
        addInstructions instructionsNew modelNew


addInstructions' :: Instruction -> Model -> Model
addInstructions' (type_, contents) model =
    case type_ of
        "getstatic" ->
            getStaticBC contents model

        "ldc" ->
            ldcBC contents model

        "invokevirtual" ->
            invokeVirtualBC contents model

        "return" ->
            returnBC contents model

        _ ->
            model


getStaticBC :: String -> Model -> Model
getStaticBC contents model =
    let
        segments = 
            splitOn ":" contents
    in
    if List.length segments == 3 then
        let
            ( constantPoolNew, indexes ) =
                addFieldRef segments (Types.constantPool model)

            methodsBCNew =
                Types.methodsBC model
                    >> putWord8 178
                    >> putWord16be (List.head indexes)
        in
        model
            { constantPool = constantPoolNew
            , methodsBC = methodsBCNew
            }
    else
        model


invokeVirtualBC :: String -> Model -> Model
invokeVirtualBC contents model =
    let
        segments = 
            splitOn ":" contents
    in
    if List.length segments == 3 then
        let
            ( constantPoolNew, indexes ) =
                addMethodRef segments (Types.constantPool model)

            methodsBCNew =
                Types.methodsBC model
                    >> putWord8 182
                    >> putWord16be (List.head indexes)
        in
        model
            { constantPool = constantPoolNew
            , methodsBC = methodsBCNew
            }
    else
        model


ldcBC :: String -> Model -> Model
ldcBC contents model =
    let
        ( constantPoolNew, indexes ) =
            addStringRef contents (Types.constantPool model)

        methodsBCNew =
            Types.methodsBC model
                >> putWord8 18
                >> putWord8 (word16ToWord8 (List.head indexes))
    in
    model
        { constantPool = constantPoolNew
        , methodsBC = methodsBCNew
        }


returnBC :: String -> Model -> Model
returnBC contents model =
    let
        methodsBCNew =
            Types.methodsBC model
                >> putWord8 177
    in
    model
        { methodsBC = methodsBCNew
        }


instructionSize :: Instruction -> Int
instructionSize ( name, contents ) =
    case name of
        "getstatic" ->
            3

        "ldc" ->
            2

        "invokevirtual" ->
            3

        "return" ->
            1

        _ ->
            0