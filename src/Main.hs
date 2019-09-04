module Main where

import Data.ByteString.Lazy as BS
import Components (generateByteCode)

main :: IO ()
main =
    let
        components =
            [ ( "class", "HelloWorld" )
            , ( "class", "java/lang/Object" )
            , ( "class", "java/lang/System" )               -- 3
            , ( "class", "java/io/PrintStream" )            -- 4
            , ( "string", "Hello World" )
            , ( "field", "3:out:Ljava/io/PrintStream;" )
            , ( "method", "4:println:(Ljava/lang/String;)V" )
            ]

        contents =
            generateByteCode components
    in
    BS.writeFile "Test.class" contents
    >> Prelude.putStrLn "File Written"
