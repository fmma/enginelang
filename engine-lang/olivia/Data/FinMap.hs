module Data.FinMap where

import Data.Entry

newtype FinMap k v = FinMap [Entry k v]

instance Functor (FinMap k) where
    fmap f (FinMap m) = FinMap (map (fmap f) m)
