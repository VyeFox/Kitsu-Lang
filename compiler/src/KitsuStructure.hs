module KitsuStructure where

import KitsuByteCode
import KitsuPreludeConnection

data PropCheck = PropCheck {
        property :: String,
        valueread :: Maybe String,
        inner :: Maybe ObjCheck
    }

data ObjCheck = ObjCheck {
        typecheck :: Maybe String,
        propchekcs :: [PropCheck]
    }



