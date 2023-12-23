{-# LANGUAGE MultiParamTypeClasses #-}

module Exp where

import Debug.Trace

import qualified Type
import Type ( Type )
import Data.List
import Data.Char
import Data.Semigroup
import Text.ParserCombinators.ReadP
import qualified Text.Read

data Entry e = Entry String e

instance Show e => Show (Entry e) where
    show (Entry k v) = k ++ ": " ++ show v

data Exp
    = Var String
    | Number Double
    | String String
    | Boolean Bool
    | Object [Entry Exp]
    | ClassDefinition String [String] [Either (Entry Type) (Entry Exp)]
    | ObjectIndex Exp String
    | Lambda String Exp
    | App Exp Exp
    | Let String Exp Exp
    | LetRec [(String, Type, Exp)] Exp
    | List [Exp]
    | TypeAnnotation Exp Type
    | DumpContext

gatherLets :: Exp -> (Exp, [(String, Exp)])
gatherLets e =
    case e of
        Let x e0 e1 -> (e1, [(x, e0)])-- (:) (x, e0) <$> gatherLets e1
        _ -> (e, [])

instance Read Exp where
    readsPrec _ = readP_to_S parseExp

parseExp :: ReadP Exp
parseExp = parseStmt
    where
        parseClassDef = do
            skipSpaces
            n <- parseSymbol
            as <- many (satisfy isSpace *> skipSpaces *> parseVar)
            skipSpaces
            char '{'
            ets <- many parseClassEntry
            skipSpaces
            char '}'
            return $ ClassDefinition n as ets
        parseClassEntry = ((Left <$> parseTypeEntry) <++ (Right <$> parseEntry))
        parseStmt = (do
            x <- parseVar
            xs <- satisfy isSpace *> sepBy parseVar (satisfy isSpace)
            skipSpaces
            char '='
            e0 <- parseAnn
            skipSpaces
            char ';'
            e1 <- parseStmt
            let e0' = foldr Lambda e0 xs
            return $ Let x e0' e1
            ) <++ parseAnn
        parseAnn = (TypeAnnotation <$> parseLambda <*> (skipSpaces *> char ':' *> Type.parseType)) <++ parseLambda
        parseLambda = (do
            xs <- sepBy1 parseVar (satisfy isSpace)
            skipSpaces
            string "=>"
            e <- parseLambda
            return $ foldr Lambda e xs) <++ parseApp
        parseApp = foldl1 App <$> sepBy1 parseIndex (satisfy isSpace <* skipSpaces)
        parseIndex = do
            obj <- parseAtom
            is <- many (skipSpaces *> char '.' *> parseVar)
            return $ foldl ObjectIndex obj is
        parseAtom = parseParens <++ parseList <++ parseObject <++ parseDumpContext <++ parseLiteral <++ (Var <$> parseVar)
        parseLiteral = skipSpaces *> (Number <$> parseNumberLiteral) <++ (String <$> parseStringLiteral) <++ (Boolean <$> parseBooleanLiteral)
        parseVar = skipSpaces *> ((:) <$> satisfy isLower <*> many (satisfy isAlphaNum))
        parseSymbol = skipSpaces *> ((:) <$> satisfy isUpper <*> many (satisfy isAlphaNum))
        parseParens = skipSpaces *> char '(' *> parseExp <* char ')'
        parseList = skipSpaces *> char '[' *> (List <$> sepBy parseExp (skipSpaces *> char ',')) <* char ']'
        parseObject = skipSpaces *> char '{' *> (Object <$> many parseEntry) <* skipSpaces <* char '}'
        parseEntry = do
            xs <- sepBy1 parseVar (satisfy isSpace)
            skipSpaces
            string "="
            e <- parseExp
            skipSpaces
            char ';'
            return $ Entry (head xs) $ foldr Lambda e (tail xs)
        parseTypeEntry = do
            x <- parseVar
            skipSpaces
            string "="
            t <- Type.parseType
            skipSpaces
            char ';'
            return $ Entry x $ t
        parseDumpContext = const DumpContext <$> (skipSpaces *> char '~')

parseNumberLiteral :: ReadP Double
parseNumberLiteral = do
    skipSpaces
    n <- ((++) <$> many1 (satisfy isDigit) <*> ( (:) <$> char '.' <*> many1 (satisfy isDigit))) <++ many1 (satisfy isDigit)
    return $ read n

parseStringLiteral :: ReadP String
parseStringLiteral = do
    skipSpaces
    char '"'
    x <- concat <$> many (string "\\\"" <++ (pure <$> satisfy (/= '"')))
    char '"'
    return x

parseBooleanLiteral :: ReadP Bool
parseBooleanLiteral = (string "true" *> pure True) <++ (string "false" *> pure False)

instance Show Exp where
    showsPrec p e =
        case e of
            Var x -> (++) x
            Number d -> (++) (show d)
            String x -> (++) ("\"" ++ x ++ "\"")
            Boolean b -> (++) (if b then "true" else "false")
            Lambda x e0 -> showParen (p > arrow_prec) (showsPrec (arrow_prec + 1) (Var x) . (" => "++) . showsPrec arrow_prec e0)
            App e1 e2 -> showParen (p > app_prec) (showsPrec app_prec e1 . (" "++) . showsPrec (app_prec + 1) e2)
            ObjectIndex e0 x -> showParen (p > index_prec) (showsPrec index_prec e0 . ("."++) . showsPrec (index_prec + 1) (Var x))
            Object [] -> (++) "{}"
            Object es -> ("{"++) . foldl1 (\ a b -> b . (", "++) . a ) (map (showsPrec 0) $ es) . ("}"++)
            List [] -> (++) "[]"
            List es -> ("["++) . foldl1 (\ a b -> b . (", "++) . a ) (map (showsPrec 0) $ es) . ("]"++)
            TypeAnnotation e0 t -> showParen (p > ann_prec) (showsPrec ann_prec e0 . (" : "++) . showsPrec 11 t)
            Let x e0 e1 -> showParen (p > stmt_prec) (showsPrec stmt_prec (Var x) . (" = "++) . showsPrec 0 e0 . (";\n"++) . showsPrec stmt_prec e1)
            DumpContext -> ("~"++)
            ClassDefinition n as ets -> ((++) (intercalate ", " (n:as))) . ("{"++) . ("}"++)
        where
            index_prec = 11
            app_prec = 10
            arrow_prec = 5
            ann_prec = 3
            stmt_prec = 0

data CstrResult a = CstrResult {
    cstrResultNewVars :: Int,
    cstrResultContext :: Type.Context,
    cstrResultValue :: Either String a
}

newtype Cstr a = Cstr (Type.Context -> Int -> CstrResult a)

runCstr (Cstr f) =
    let w = f Type.idContext 0
    in cstrResultValue w

fresh :: Cstr Int
fresh = Cstr $ \ w i -> CstrResult 1 w (Right i)

(=:=) :: Type -> Type -> Cstr ()
t1 =:= t2 = Cstr $ \ w i -> case Type.unify t1 t2 w of
    Right w' -> CstrResult 0 w' (Right ());
    Left err -> CstrResult 0 w (Left err)

returnType :: Type.Type -> Cstr Type.Type
returnType t = Cstr $ \ w _ -> CstrResult 0 w (Right (Type.subst w t))

getSubstitution :: Cstr Type.Context
getSubstitution = Cstr $ \ w _ -> CstrResult 0 w (Right w)

var :: Cstr Type
var = do
    i <- fresh
    return $ Type.Var ("x" ++ show i)

instance Functor CstrResult where
    fmap f (CstrResult i w a) = CstrResult i w (fmap f a)

instance Functor Cstr where
    fmap f (Cstr c) = Cstr $ \ w i -> fmap f (c w i)

instance Applicative Cstr where
    pure x = liftEither (pure x)
    Cstr f <*> Cstr x = Cstr $ \ w i ->
        let CstrResult i1 w1 f' = f w i
            CstrResult i2 w2 x' = x w1 (i + i1)
        in CstrResult (i1 + i2) w2 (f' <*> x')

liftEither :: Either String a -> Cstr a
liftEither x = Cstr $ \ w i -> CstrResult 0 w x

instance Monad Cstr where
    return = pure
    Cstr x >>= f = Cstr $ \ w i ->
        let CstrResult i1 w1 x' = x w i
        in case x' of
            Left err -> CstrResult i1 w1 (Left err)
            Right x' ->
                let Cstr g = f x'
                    CstrResult i2 w2 y = g w1 (i + i1)
                in CstrResult (i1 + i2) w2 y

data Context
    = Empty
    | Append Context Context
    | Mapping String Type

instantiate :: Type -> Cstr Type
instantiate t =
    case t of
        Type.Forall x t0 -> do
            tx <- var
            Type.substSingle x tx <$> instantiate t0
        _ -> pure t

annotationOrVar :: Entry Exp -> Cstr (Entry (Exp, Type))
annotationOrVar (Entry x e) =
    case e of
        TypeAnnotation _ t -> pure (Entry x (e, t))
        _ -> do
            t <- var
            return (Entry x (e, t))

typeInference :: [String] -> Type -> Exp -> Cstr Type
typeInference tvs g e =
    returnType =<<
    case e of
        Var x ->
            case Type.lookupObject g x of
                Nothing -> liftEither $ Left $ "Unbound variable: " ++ x
                Just t -> instantiate t
        Number _ -> return Type.number
        String _ -> return Type.string
        Boolean _ -> return Type.boolean
        Object es -> do
            ets <- mapM annotationOrVar es
            let g' = foldl (\ g0 (Entry x (_, t0)) -> update g0 x t0) g ets
            mapM_ (\ (Entry _ (e0, t0)) -> do
                t0' <- typeInference tvs g' e0
                t0 =:= t0') ets
            w <- getSubstitution
            let fs = Type.vars (Type.subst w g)
                ts' = map (\ (Entry x (_, t0)) -> Entry x (Type.subst w t0)) ets
                vs = concatMap (\ (Entry _ t0) -> Type.vars t0) ts'
                foralls = nub vs \\ fs
                ts'' = map (\ (Entry x t0) -> Entry x (Type.generalize foralls t0)) ts'
            return $ foldl (\ g0 (Entry x t0) -> update g0 x t0) (Type.object []) ts''
        ObjectIndex e0 x -> do
            t0 <- typeInference tvs g e0
            case Type.lookupObject t0 x of
                Nothing -> liftEither $ Left $ if Type.isCertainlyObject t0 then "Object " ++ show t0 ++ " does not contain key " ++ x else "Cannot infer object type from member access " ++ show t0 ++ "." ++ x
                Just t1 -> instantiate t1
        Lambda x e0 -> do
            t1 <- var
            t2 <- typeInference tvs (update g x t1) e0
            return $ Type.function t1 t2
        App e0 e1 -> do
            t0 <- typeInference tvs g e0
            t1 <- typeInference tvs g e1
            t2 <- var
            t0 =:= Type.function t1 t2
            return t2
        Let x e0 e1 -> do
            t0 <- typeInference tvs g e0
            w <- getSubstitution
            let fs = Type.vars (Type.subst w g)
                t0' = Type.subst w t0
                vs = Type.vars t0'
                foralls = nub vs \\ fs
                t0'' = Type.generalize foralls t0'
            typeInference tvs (update g x t0'') e1
        List es -> do
            t <- var
            ts <- mapM (typeInference tvs g) es
            mapM (t =:=) ts
            return $ Type.list t
        TypeAnnotation e0 t -> do
            case nub (Type.vars t) \\ tvs of
                [] -> do
                    t' <- instantiate t
                    let qtvs = Type.schemeVars t
                    t0 <- typeInference (tvs ++ qtvs) g e0
                    t' =:= t0
                    case qtvs of
                        (_:_) -> do
                            w <- getSubstitution
                            let fs = Type.vars (Type.subst w g)
                                t0' = Type.subst w t0
                                vs = Type.vars t0'
                                foralls = nub vs \\ fs
                                t0'' = Type.generalize foralls t0'
                            t =:= t0''
                        _ -> return ()
                    return t
                xs -> liftEither $ Left $ "Unbound type variable(s) " ++ intercalate ", "  xs ++ " in " ++ show t
        DumpContext -> pure g

update :: Type -> String -> Type -> Type
update g x t = Type.objectAppendEntry g x t
