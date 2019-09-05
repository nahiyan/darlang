module Main where

import Data.ByteString.Lazy as BS
import ClassFile (writeClassFile)

main :: IO ()
main =
    let
        methods_ =
            [ Method
                { methodName = "main"
                , accessType = "public static"
                , descriptor = "([Ljava/lang/String;)V"
                , maxStackSize = 2
                , maxLocalVarSize = 1
                , code =
                    [ ( "getstatic", "java/lang/System:out:Ljava/io/PrintStream" )
                    , ( "ldc", "Hello World!" )
                    , ( "invokevirtual", "java/io/PrintStream:printLn:(Ljava/lang/String;)V")
                    ]
                }
            ]

        class_ =
            Class
                { className = "HelloWorld"
                , methods = methods_
                }

        -- components =
        --     [ ( "class_ref", "HelloWorld" )
        --     , ( "class_ref", "java/lang/Object" )
        --     , ( "class_ref", "java/lang/System" )               -- 3
        --     , ( "class_ref", "java/io/PrintStream" )            -- 4
        --     , ( "string_ref", "Hello World" )
        --     , ( "field_ref", "3:out:Ljava/io/PrintStream;" )
        --     , ( "method_ref", "4:println:(Ljava/lang/String;)V" )
        --     , ( "method", "main:([Ljava/lang/String;)V" )
        --     ]
    in
    writeClassFile "Test" class_
