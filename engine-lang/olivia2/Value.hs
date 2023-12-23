module Value where

import Exp

data Value
    = Number Double
    | String String
    | Boolean Bool
    | Object [(String, Value)]
    | Lambda String [(Value, Value)] Exp
    | List [Value]
