module Main where

import Data.ByteString.Lazy as BS
import ClassFile (writeClassFile)

import Types (Class(..), Method(..))

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
                    [ ( "getstatic", "java/lang/System:out:Ljava/io/PrintStream;" )
                    , ( "ldc", "Hello World!" )
                    , ( "invokevirtual", "java/io/PrintStream:println:(Ljava/lang/String;)V")
                    , ( "return", "" )
                    ]
                }
            ]

        class_ =
            Class
                { className = "HelloWorld"
                , methods = methods_
                }
    in
    writeClassFile class_
