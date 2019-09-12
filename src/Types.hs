module Types where

import           Data.Binary.Put ( Put )
import           Data.Word       ( Word16 )

data Method =
    Method { methodName      :: String
           , accessType      :: String
           , descriptor      :: String
           , maxStackSize    :: Word16
           , maxLocalVarSize :: Word16
           , code            :: [Instruction]
           }

data Class =
    Class { className :: String
          , methods   :: [Method]
          }

data Model =
    Model { constantPool
                :: [( String, String )]
          , constantPoolBC :: Put
          , methodsBC      :: Put
          }

type Instruction =
    ( String, String )

type CPItem =
    ( String, String )

type ConstantPool =
    [( String, String )]
