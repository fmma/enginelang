module Data.Exp where

import Data.FinMap
import Data.Var

data Exp t
    = Number Double
    | String String
    | Boolean Bool
    | Variable (Var t)
    | Let (Var t) (Exp t) (Exp t)
    | Lambda (Var t) (Exp t)
    | App (Exp t) (Exp t)
    | List [Exp t]
    | Object (FinMap (Var t) (Exp t))
    | ObjectIndex (Exp t) (Var t)
