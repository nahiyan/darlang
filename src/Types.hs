module Types where

import Data.Word (Word16)
import Data.Binary.Put (Put)


data Method =
    Method
        { methodName        :: String
        , accessType        :: String
        , descriptor        :: [ String ]
        }


data Class =
    Class
        { className     :: String
        , methods       :: [ Method ]
        }


data Model =
    Model
        { constantPool      :: [ ( String, String ) ]
        , constantPoolBC    :: Put
        , methodsBC         :: Put
        }