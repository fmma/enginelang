module Main where

import Parser
import Gadt

data Type a where
    Tfloat :: Type Float
    Tprod  :: Type a -> Type b -> Type (a, b)
    Tseq   :: Type a -> Type [a]

tFst :: Type (a, b) -> Type a
tFst (Tprod a _) = a
    
tSnd :: Type (a, b) -> Type b
tSnd (Tprod _ b) = b

tElt :: Type [a] -> Type a
tElt (Tseq a) = a

typeRefl :: Type a -> Type b -> Maybe (Refl a b)
typeRefl Tfloat Tfloat = Just Refl
typeRefl (Tprod t1 t2) (Tprod ta tb)
    | Just Refl <- typeRefl t1 ta
    , Just Refl <- typeRefl t2 tb
    = Just Refl
typeRefl _ _ = Nothing

class Expr exp where
    interp :: a -> exp a b -> b
    typeForward :: Type a -> exp a b -> Type b
    typeBackward :: Type b -> exp a b -> Type a
    parse :: Type a -> Parser (Exists (exp a))
    pretty :: exp a b -> String

instance Expr Refl where
    interp x Refl = x
    typeForward x Refl = x
    typeBackward x Refl = x
    parse _ = tok "id" >> return (Exists Refl)
    pretty _ = "id"

{-
instance Expr Pair where
    interp x (Pair e0 e1) = (interp x e0, interp x e1)
    typeForward x (Pair e0 e1) = Tprod (typeForward x e0) (typeForward x e1)
    typeBackward x (Pair e0 _) = typeBackward (tFst x) e0
    parse x = do 
        token "pair"
        e0' <- _parse x
        case e0' of
            Exists e0 -> do
                e1' <- _parse x
                case e1' of
                    Exists e1 -> return (Exists (Pair e0 e1))

data Pair a b where
    Pair :: (Expr e0, Expr e1) => e0 a b -> e1 a c -> Pair a (b, c) 
-}


main :: IO ()
main = do
    putStrLn "Bye"
