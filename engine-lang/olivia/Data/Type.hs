module Data.Type where

import Data.Entry
import Data.FinMap
import Data.Ident

data Type
    = Symbol Ident
    | Variable Ident
    | App Type Type
    | Object (FinMap Ident Type)
