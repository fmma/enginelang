module Oli where

import Data.List
import Data.Char
import Text.ParserCombinators.ReadP hiding ( string )
import qualified Text.ParserCombinators.ReadP as ReadP
import Control.Monad.Reader

data Type
    = Var String
    | Symbol String
    | App Type Type
    | Object [(String, Type)]
    deriving (Eq, Ord)

data Scheme = Scheme [String] Type

data Var
    = UntypedVar String
    | TypedVar String Type

varName :: Var -> String
varName x =
    case x of
        UntypedVar x -> x
        TypedVar x _ -> x

data Exp
    = Var' Var
    | Number' Double
    | String' String
    | Boolean' Bool
    | App' Exp Exp
    | Lambda' Var Exp
    | Let' Var Exp Exp
    | Object' [(Var, Exp)]

number :: Type
number = Symbol "Number"

string :: Type
string = Symbol "String"

boolean :: Type
boolean = Symbol "Boolean"

list :: Type -> Type
list t0 = Symbol "List" `App` t0

function :: Type -> Type -> Type
function t1 t2 = Symbol "Function" `App` t1 `App` t2

matchFunction :: Type -> Maybe (Type, Type)
matchFunction (Symbol "Function" `App` t1 `App` t2) = Just (t1, t2)
matchFunction _ = Nothing

parseNumberLiteral :: ReadP Double
parseNumberLiteral = do
    skipSpaces
    n <- ((++) <$> many1 (satisfy isDigit) <*> ( (:) <$> char '.' <*> many1 (satisfy isDigit))) <++ many1 (satisfy isDigit)
    return $ read n

parseStringLiteral :: ReadP String
parseStringLiteral = do
    skipSpaces
    char '"'
    x <- concat <$> many (ReadP.string "\\\"" <++ (pure <$> satisfy (/= '"')))
    char '"'
    return x

token :: String -> ReadP String
token cs = do
    skipSpaces
    ReadP.string cs

parseBooleanLiteral :: ReadP Bool
parseBooleanLiteral = (token "true" *> pure True) <++ (token "false" *> pure False)

parseVar :: ReadP String
parseVar = skipSpaces *> ((:) <$> satisfy isLower <*> many (satisfy isAlphaNum))

parseSymbol :: ReadP String
parseSymbol = skipSpaces *> ((:) <$> satisfy isUpper <*> many (satisfy isAlphaNum))

parseVarOrSymbol :: ReadP String
parseVarOrSymbol = skipSpaces *> ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum))

parseParens :: ReadP a -> ReadP a
parseParens p = token "(" *> p <* token ")"

parseParensSquare :: ReadP a -> ReadP a
parseParensSquare p = token "[" *> p <* token "]"

parseParensCurly :: ReadP a -> ReadP a
parseParensCurly p = token "{" *> p <* token "}"

parseCommaSeperated :: ReadP a -> ReadP [a]
parseCommaSeperated p = sepBy p (token ",")

parseSemicolonEnded :: ReadP a -> ReadP [a]
parseSemicolonEnded p = many (p <* token ";")

parseList :: ReadP a -> ReadP [a]
parseList p = parseParensSquare (parseCommaSeperated p)

parseObject :: ReadP a -> ReadP [a]
parseObject p = parseParensCurly (parseSemicolonEnded p)

parseWhitespaceSeperated :: ReadP a -> ReadP [a]
parseWhitespaceSeperated p = sepBy p (satisfy isSpace)

parseWhitespaceSeperated1 :: ReadP a -> ReadP [a]
parseWhitespaceSeperated1 p = sepBy1 p (satisfy isSpace)

runReadP :: ReadP a -> String -> [(a, String)]
runReadP p = readP_to_S (p <* skipSpaces)

runReadPEither :: Show a => ReadP a -> String -> Either String a
runReadPEither p s =
    case runReadP p s of
        [] -> Left "No parse"
        [(a, "")] -> Right a
        x -> Left ("Parse error:\n" ++ unlines (map (\ (x, y) -> show x ++ "\t\tUnparsed: " ++ show y) x))

runReadPError :: Show a => ReadP a -> String -> a
runReadPError p s =
    case runReadPEither p s of
        Left err -> error err
        Right a -> a

parseType :: ReadP Type
parseType = parseFunctionType

parseFunctionType :: ReadP Type
showFunctionType :: Type -> Type -> PrettyPrinter
functionTypePrecedence = 100
parseFunctionType = do
    ts <- sepBy1 parseAppType (token "=>")
    return $ foldr1 function ts
showFunctionType t0 t1 = setPrecedence functionTypePrecedence $ do
    s0 <- stepPrecedence $ showType t0
    s1 <- showType t1
    return $ s0 ++ " => " ++ s1

parseAppType :: ReadP Type
showAppType :: Type -> Type -> PrettyPrinter
appTypePrecedence = 101
parseAppType = do
    ts <- parseWhitespaceSeperated1 parseAtomType
    return $ foldl1 App ts
showAppType t0 t1 = setPrecedence appTypePrecedence $ do
    s0 <- showType t0
    s1 <- stepPrecedence $ showType t1
    return $ s0 ++ " " ++ s1

parseAtomType :: ReadP Type
parseAtomType = skipSpaces *> (fmap Var parseVar <++ fmap Symbol parseSymbol <++ parseParens parseType <++ parseListType)

parseListType :: ReadP Type
showListType :: Type -> PrettyPrinter
parseListType = do
    t <- parseParensSquare parseType
    return $ list t
showListType t = do
    s <- resetPrecedence $ showType t
    return $ "[" ++ s ++ "]"

type PrettyPrinter = Reader (String, Int) String

runPrettyPrinter :: PrettyPrinter -> String
runPrettyPrinter pp = runReader pp ("", 0)

showParens :: Int -> PrettyPrinter -> PrettyPrinter
showParens n pp = do
    (_, m) <- ask
    if m > n
    then do
        s <- pp
        return $ "(" ++ s ++ ")"
    else pp

setPrecedence :: Int -> PrettyPrinter -> PrettyPrinter
setPrecedence n pp =
    showParens n $ local (\ (indent, _) -> (indent, n)) pp

stepPrecedence :: PrettyPrinter -> PrettyPrinter
stepPrecedence pp = local (\ (indent, n) -> (indent, n + 1)) pp

resetPrecedence :: PrettyPrinter -> PrettyPrinter
resetPrecedence = setPrecedence 0

showType :: Type -> PrettyPrinter
showType t =
    case t of
        Symbol "List" `App` t0 -> showListType t0
        Symbol "Function" `App` t1 `App` t2 -> showFunctionType t1 t2
        Var x -> return x
        Symbol x -> return x
        App t1 t2 -> showAppType t1 t2

instance Read Type where
    readsPrec _ s = runReadP parseType s

instance Show Type where
    show t = runPrettyPrinter (showType t)

getType :: Exp -> Maybe Type
getType e =
    case e of
        Var' (TypedVar _ t) -> Just t
        Number' _ -> Just number
        String' _ -> Just string
        Boolean' _ -> Just boolean
        App' e0 _ -> do
            t0 <- getType e0
            (_, t2) <- matchFunction t0
            return t2
        Lambda' (TypedVar _ t) e0 -> function t <$> getType e0
        Let' _ _ e1 -> getType e1
        Object' ets -> Object <$> mapM getTypeEntry ets
        _ -> Nothing

getTypeEntry :: (Var, Exp) -> Maybe (String, Type)
getTypeEntry (TypedVar x t, _) = Just (x, t)
getTypeEntry (UntypedVar x, e) = (,) x <$> getType e

getTypeOrVar :: Exp -> Infer Exp
getTypeOrVar e =
    case getType e of
        Nothing -> freshVar
        Just t -> return t

typeInference :: [String] -> Type -> Exp -> Type -> Infer Exp
typeInference typeVars context e t =
    case e of
        Var' x -> typeInference typeVars context x t
        Number' x -> return (Number' x)
        String' x -> return (String' x)
        Boolean' x -> return (Boolean' x)
        App' e0 e1 -> do
            t1 <- getTypeOrVar e1
            e1' <- typeInference typeVars context e1 t1
            e0' <- typeInference typeVars context e0 (function t1 t)
            return $ App e0' e1'

lookupVarType :: Exp -> String -> Maybe Type
lookupVarType e x =
    case e of
        Var' (TypedVar y t) | x == y -> Just t
        App' e0 e1 -> lookupVarType e0 x <|> lookupVarType e1 x
        Lambda' y e0
            | x == y -> Nothing
            | otherwise -> lookupVarType e0 x
        Let' y e0 e1 -> _
        Object' ets -> _
