module Main where

import Data.Char
import Data.List
import Parser

main :: IO ()
main = do
    l <- getLine
    case l of
        "q" -> return ()
        _ -> case runParser (parse :: Parser Exp) l of
            e -> do 
                putStrLn (show e)
                case e of 
                    (e',_):_ -> putStrLn $ "HASKELL:\n" ++ (transpileToHaskell e')
                    _ -> putStrLn $ "no haskell"
                main


newtype Var = Var String
    deriving Show

data Param = Param Type Var

data Exp
    = ExpVar Var
    | BinOp Var Exp Exp -- + - * / % ^ && || == > < >= <=
    | Builtin Var  -- - abs signum exp log logBase sqrt ceil floor round sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh !
    | App Exp Exp
    | IntConstant Int
    | FloatConstant Float
    deriving Show

data Type
    = TypeVar Var
    | TypeConstructor String [Type]

data Poly a
    = Forall Var Kind (Poly Type)
    | Dot a

data Command
    = Assign Var Exp
    | Exp
    | Return Exp

-- Generalized algebraic class
data Gac = Gac Var (Poly Type) [Prop]

data Prop 
    = SubGac Gac
    | Signature Var (Poly Type)
    | Definition Var Exp

data Kind = Ktype | Kfun Kind Kind

{-
Tree a {

}

Tree a {
    fold : a -> (a -> a -> a) -> a;
    pairWith : Tree a -> Tree a;

    pairOn : NonEmptyTree a -> Tree a;

    size : Int
    size => fold 0 [a b. size a + size b];

    Empty a : Tree a {
        fold z f => z;
        pairWith t => t;
        pairOn t => t;
    }

    NonEmptyTreeTree a : Tree a {
        pairWith t => t.pairOn this;
        pairOn t => Pair a t this;

        Leaf a (a : a) : NonEmptyTree a {
            fold z f => a;
        }

        Pair a (left : NonEmptyTree a, right: NonEmptyTree a) : NonEmptyTree a {
            fold z f => f (left.fold f) (right.fold f);
        }
    }
}

data Tree a where
    Unit :: Tree ()
    Pair :: a -> b -> Tree (a, b)
-}

class Parse a where
    parse :: Parser a
    parse = parsePrec ""

    parsePrec :: String -> Parser a
    parsePrec _ = parse

    runParse :: String -> Maybe a
    runParse src =
        case runParser (parse <* parseWhitespace) src of
            [(r, [])] -> Just r
            _ -> Nothing

instance Parse Int where
    parse = do 
        guard (\ w -> not $ "-" `isPrefixOf` w)
        parseInt

instance Parse Float where
    parse = do 
        guard (\ w -> not $ "-" `isPrefixOf` w)
        parseFloat

instance Parse String where
    parse = do 
        _ <- parseWhitespace
        (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)

instance Parse Var where
    parse = Var <$> parse

instance Parse Exp where
    parsePrec "||" =
        chainl (BinOp . Var <$> wtok "||")
        (parsePrec "&&")
    parsePrec "&&" =
        chainl (BinOp . Var <$> wtok "&&")
        (parsePrec "<")
    parsePrec "<" =
        chainl (BinOp . Var <$> wtoks ["<=", ">=", "==", "<", ">"]) 
        (parsePrec "+")
    parsePrec "+" =
        chainl (BinOp . Var <$> wtoks ["+", "-"])
        (parsePrec "*")
    parsePrec "*" =
        chainl (BinOp . Var <$> wtoks ["*", "/", "%"])
        (parsePrec "^")
    parsePrec "^" =
        chainr (BinOp . Var <$> wtoks ["^"]) 
        (parsePrec "app")
    parsePrec "app" =
        chainl (const App <$> parseWhitespace)
        (parsePrec "prefix")
    parsePrec "prefix" = do
        o <- Just <$> wtoks ["-"] <|> return Nothing
        case o of
            Just prefix -> do 
                guard (\ w -> not $ " " `isPrefixOf` w)
                a <- parsePrec "prefix"
                return $ App (Builtin (Var ("prefix" ++ prefix))) a
            Nothing -> parsePrec "atom"
    parsePrec "atom" =
        wtok "(" *> parse <* wtok ")" <|>
        IntConstant <$> parse <|>
        FloatConstant <$> parse <|>
        ExpVar <$> parse <|>
        Builtin . Var <$> wtoks ops
        where 
            ops = ["abs", "signum", "exp", "log", "logBase", "sqrt", "ceil", "floor", "round", 
                "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh", "asinh", "acosh", "atanh", "!"]
    parsePrec _ =
        parsePrec "||"

class Transpile e where
    transpileToHaskell :: e -> String

instance Transpile Var where
    transpileToHaskell (Var x) = 
        case x of
            "prefix-" -> "negate"
            "!" -> "not"
            _ -> x

instance Transpile Int where
    transpileToHaskell x
        | x < 0 = "(" ++ show x ++ ")"
        | otherwise = show x

instance Transpile Float where
    transpileToHaskell x = show x

instance Transpile Exp where
    transpileToHaskell e = 
        case e of
            ExpVar x -> transpileToHaskell x
            BinOp o e1 e2 -> "(" ++ transpileToHaskell e1 ++ " " ++ transpileToHaskell o ++ " " ++ transpileToHaskell e2 ++ ")"
            Builtin o -> transpileToHaskell o
            App e1 e2 -> "(" ++ transpileToHaskell e1 ++ " " ++ transpileToHaskell e2 ++ ")"
            IntConstant n -> transpileToHaskell n
            FloatConstant n -> transpileToHaskell n

{-

data Foo a b c where
    Bar :: a -> Baz b -> Foo a b b

-}