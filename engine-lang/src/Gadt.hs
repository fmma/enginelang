module Gadt where

data Exists a where
    Exists :: a x -> Exists a

data Exists2 a where
    Exists2 :: a x y -> Exists2 a

data Refl a b where
    Refl :: Refl a a
