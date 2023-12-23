module Data.Entry where

import Control.Parse

data Entry k v = Entry k v

instance Functor (Entry k) where
    fmap f (Entry k v) = Entry k (f v)

instance (Show k, Show v) => Show (Entry k v) where
    show (Entry k v) = show k ++ " = " ++ show v

instance (Parse k, Parse v) => Parse (Entry k v) where
    parse = do
        k <- parse
        v <- parse
        return $ Entry k v
