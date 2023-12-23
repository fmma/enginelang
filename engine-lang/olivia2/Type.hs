module Type where

import Data.List
import Data.Char
import Text.ParserCombinators.ReadP
import qualified Text.Read

data Type
    = Var String
    | Symbol String
    | App Type Type
    | Forall String Type
    deriving (Eq, Ord)

generalize :: [String] -> Type -> Type
generalize xs t =
    let ys = vars t
    in foldr Forall t (nub $ [x | x <- xs, x `elem` ys])

number :: Type
number = Symbol "Number"

string :: Type
string = Symbol "String"

boolean :: Type
boolean = Symbol "Boolean"

object :: [(String, Type)] -> Type
object ts = foldl App (Symbol "Object") (map (\ (k, t) -> Symbol "Entry" `App` Symbol k `App` t) $ sort ts)

objectAppendEntry :: Type -> String -> Type -> Type
objectAppendEntry t x t0 = t `App` (Symbol "Entry" `App` Symbol x `App` t0)

function :: Type -> Type -> Type
function t1 t2 = Symbol "Function" `App` t1 `App` t2

list :: Type -> Type
list t0 = Symbol "List" `App` t0

lookupObject :: Type -> String -> Maybe Type
lookupObject g x =
    case g of
        t0 `App` (Symbol "Entry" `App` Symbol y `App` t1)
            | x == y -> Just t1
            | otherwise -> lookupObject t0 x
        _ -> Nothing

isCertainlyObject :: Type -> Bool
isCertainlyObject t =
    case t of
        Symbol "Object" -> True
        _ `App` (Symbol "Entry" `App` Symbol _ `App` _) -> True
        _ -> False

type Context = String -> Type
type Update = Context -> Either String Context

instance Read Type where
    readsPrec _ = readP_to_S parseType

parseType :: ReadP Type
parseType = parseForall <* skipSpaces
    where
        parseVar = Var <$> ((:) <$> satisfy isLower <*> many (satisfy isAlphaNum))
        parseSymbol = Symbol <$> ((:) <$> satisfy isUpper <*> many (satisfy isAlphaNum))
        parseSymbolAnyCase = Symbol <$> many1 (satisfy isAlphaNum)
        parseList = App (Symbol "List") <$> (char '[' *> parseType  <* char ']')
        parseObject = foldl App (Symbol "Object") . sort <$> (char '{' *> sepBy parseEntry (char ',')  <* char '}')
        parseEntry = (\ t1 t2 -> Symbol "Entry" `App` t1 `App` t2) <$> (skipSpaces *> parseSymbolAnyCase <* skipSpaces <* char ':') <*> parseType
        parseParens = char '(' *> parseType <* char ')'
        parseFunction = foldr1 (\ t1 t2 -> Symbol "Function" `App` t1 `App` t2) <$> sepBy1 parseApp (skipSpaces *> Text.ParserCombinators.ReadP.string "=>")
        parseApp = foldl1 App <$> sepBy parseAtom (satisfy isSpace <* skipSpaces)
        parseAtom = skipSpaces *> (parseVar <++ parseSymbol <++ parseParens <++ parseList <++ parseObject)
        parseForall = (do
            skipSpaces
            Text.ParserCombinators.ReadP.string "forall"
            xs <- map (\ (Var x) -> x) <$> (many1 (satisfy isSpace *> skipSpaces *> parseVar))
            char '.'
            t <- parseFunction
            return $ foldr Forall t xs) <++ parseFunction

collectArguments :: Type -> [Type]
collectArguments t =
    case t of
        App t1 t2 -> t2 : collectArguments t1
        _ -> []

instance Show Type where
    showsPrec p t =
        case t of
            Symbol "List" `App` t0 -> ("["++) . showsPrec 0 t0 . ("]"++)
            Symbol "Function" `App` t1 `App` t2 -> showParen (p > arrow_prec) (showsPrec (arrow_prec + 1) t1 . (" => "++) . showsPrec arrow_prec t2)
            Symbol "Object" -> ("{}"++)
            Symbol "Entry" `App` t1 `App` t2 -> showsPrec 0 t1 . (": "++) . showsPrec 0 t2
            App t1 (Symbol "Entry" `App` _ `App` _) -> ("{"++) . foldl1 (\ a b -> b . (", "++) . a ) (map (showsPrec 0) $ collectArguments t) . ("}"++)
            Var x -> (x ++)
            Symbol x -> (x ++)
            App t1 t2 -> showParen (p > app_prec) (showsPrec app_prec t1 . (" "++) . showsPrec (app_prec + 1) t2)
            Forall x t0 -> (++) ("forall " ++ x ++ ". ") . showsPrec 0 t0
        where app_prec = 10
              arrow_prec = 5

vars :: Type -> [String]
vars t =
    case t of
        Var x -> [x]
        Symbol _ -> []
        App t1 t2 -> vars t1 ++ vars t2
        Forall x t0 -> nub (vars t0) \\ [x]

schemeVars :: Type -> [String]
schemeVars t =
    case t of
        Forall x t0 -> x : schemeVars t0
        _ -> []

subst :: Context -> Type -> Type
subst w t =
    case t of
        Var x -> w x
        Symbol f -> Symbol f
        App t1 t2 -> App (subst w t1) (subst w t2)
        Forall x t0 -> Forall x (subst (\ y -> if x == y then Var x else w y) t0)

substSingle :: String -> Type -> Type -> Type
substSingle x t0 = subst (\ y -> if x == y then t0 else Var y)

idContext :: Context
idContext = \x -> Var x

failContext :: String -> Update
failContext err _ = Left err

update :: String -> Type -> Update
update x t w = Right $ substSingle x t . w

occursCheck :: String -> Type -> Bool
occursCheck x t = x `elem` vars t

unify :: Type -> Type -> Update
unify (Forall x t0) (Forall y t1) = unify t0 (substSingle y (Var x) t1)
unify (Symbol f) (Symbol g) | f == g = pure
unify (Var x) (Var y) | x == y = pure
unify (Var x) t2 = if x `elem` vars t2
    then failContext $ "Occurs check failed: " ++ x ++ " occurs in " ++ show t2
    else update x t2
unify t1 (Var y) = unify (Var y) t1
unify (App t1 t2) (App t1' t2') =
    \w -> case unify t1 t1' w of
            Left err -> Left err
            Right w' -> unify (subst w' t2) (subst w' t2') w'
unify t1 t2 = failContext $ "Failed to unify " ++ show t1 ++ " == " ++ show t2

pprint :: Type -> String
pprint t = pprintPrec "" 0 t ""

pprintPrec :: String -> Int -> Type -> String -> String
pprintPrec idt p t =
    case t of
        Symbol "List" `App` t0 -> ("["++) . pprintPrec idt 0 t0 . ("]"++)
        Symbol "Function" `App` t1 `App` t2 -> showParen (p > arrow_prec) (pprintPrec idt (arrow_prec + 1) t1 . (" => "++) . pprintPrec idt arrow_prec t2)
        Symbol "Object" -> ("{}"++)
        Symbol "Entry" `App` t1 `App` t2 -> pprintPrec idt 0 t1 . (": "++) . pprintPrec idt 0 t2
        App t1 (Symbol "Entry" `App` _ `App` _) -> ("{\n"++) . (idt'++) . foldl1 (\ a b -> b . (",\n"++) . (idt'++) . a ) (map (pprintPrec (idt') 0) $ collectArguments t) . ("\n"++) . (idt++) . ("}"++) where idt' = idt ++ "  "
        Var x -> (x ++)
        Symbol x -> (x ++)
        App t1 t2 -> showParen (p > app_prec) (pprintPrec idt app_prec t1 . (" "++) . pprintPrec idt (app_prec + 1) t2)
        Forall x t0 -> (++) ("forall " ++ x ++ ". ") . pprintPrec idt 0 t0
    where
        app_prec = 10
        arrow_prec = 5
