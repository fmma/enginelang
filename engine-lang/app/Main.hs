module Main where

    import Data.Char
    import Data.List ( intercalate, union, sort )
    import Data.Monoid
    import Control.Monad.Fail
    import Control.Applicative
    import Control.Monad.Writer
    import Control.Monad.Except
    import System.Directory
    import System.Exit
    import System.FilePath
    import System.IO
    import System.Process
    
    import Debug.Trace
    
    trace' :: String -> a -> a
    trace' x = case False of
        True -> id
        False -> trace x
    
    data Tree e = Leaf e | Branch [Tree e] deriving Functor
    
    instance Applicative Tree where
        pure x = Leaf x
        f <*> x =
            case f of 
                Leaf f' -> f' <$> x
                Branch fs -> Branch (map (<*> x) fs)
    
    instance Monad Tree where
        return = pure
        x >>= f =
            case x of
                Leaf x' -> f x'
                Branch xs -> Branch (map (>>= f) xs)
    
    instance Show e => Show (Tree e) where
        show (Leaf x) = show x
        show (Branch xs) = show xs
    
    treeToList :: Tree a -> [a]
    treeToList t =
        case t of
            Leaf e -> [e]
            Branch ts -> concatMap treeToList ts
    
    -- begin HL
    
    -- t ::= 1 | 0 | t `|` t | t , t | float
    data Type = Tfloat |
        Tpair Type Type | Tunit |
        Teither Type Type | Tempty |
        Tvar Int | TvarExistential Int |
        Tarray Type 
        deriving (Eq)
    
    mkTupleType :: [Type] -> Type
    mkTupleType [] = Tunit
    mkTupleType [t] = t
    mkTupleType (t:ts) = Tpair t (mkTupleType ts)
    
    fstType, sndType, inlType, inrType, eltType :: Type -> Type
    fstType t = 
        case t of
            Tpair t1 _ -> t1
            _ -> error $ show t
    sndType t = 
        case t of
            Tpair _ t2 -> t2
            _ -> error $ show t
    inlType t = 
        case t of
            Teither t1 _ -> t1
            _ -> error $ show t
    inrType t = 
        case t of
            Teither _ t2 -> t2
            _ -> error $ show t
    eltType t = 
        case t of
            Tarray t0 -> t0
            _ -> error $ show t
            
    instance Show Type where
        show t' = go False t'
            where
                parens False x = x
                parens True x = "(" ++ x ++ ")"
                go b t =
                    let isComplex t0 = isPair t0 || isEither t0 
                        isPair t0 = case t0 of
                            Tpair _ _ -> True
                            _ -> False
                        isEither t0 = case t0 of
                            Teither _ _ -> True
                            _ -> False 
                    in
                    parens (b && isComplex t) $
                    case t of
                        Tfloat -> "float"
                        Tpair t1 t2 -> go True t1 ++ " * " ++ go (not (isPair t2)) t2
                        Tunit -> "1"
                        Teither t1 t2 -> go True t1 ++ " + " ++ go (not (isEither t2)) t2
                        Tempty -> "0"
                        Tvar a -> [chr (a + ord 'a')]
                        TvarExistential a -> [chr (a + ord 'a')]
                        Tarray t0 -> "[" ++ go False t0 ++ "]"
    
    data Value = Vfloat Float | Vpair Value Value | Vunit | Vinl Value | Vinr Value | Varray [Value]
    
    isTvar :: Type -> Bool
    isTvar t =
        case t of
            Tvar _ -> True
            _ -> False
    
    data ExpAxiom where
        ExpNum :: Float -> ExpAxiom
        ExpUnit :: ExpAxiom
        ExpEmpty :: ExpAxiom
        ExpFst :: ExpAxiom
        ExpSnd :: ExpAxiom
        ExpInl :: ExpAxiom
        ExpInr :: ExpAxiom
        ExpDist :: ExpAxiom
        ExpMinus :: ExpAxiom
        ExpPlus :: ExpAxiom
        ExpTimes :: ExpAxiom
        ExpEquals :: ExpAxiom
        ExpVar :: ExpAxiom
        ExpIota :: ExpAxiom
        ExpMkList :: Int -> ExpAxiom
        ExpLength :: ExpAxiom
        ExpHead :: ExpAxiom
        ExpTake :: ExpAxiom
        ExpDrop :: ExpAxiom
        ExpEmptyArray :: ExpAxiom
        ExpAppend :: ExpAxiom
        ExpStr :: ExpAxiom
    
    -- e ::= e , e | unit | e `|` e | empty | fst | snd | inl | inr | dist | x | e e | float | - | + | * | ==
    data Exp e where
        ExpApp :: e -> e -> Exp e
        ExpPair :: e -> e -> Exp e
        ExpEither :: e -> e -> Exp e
        ExpAxiom :: ExpAxiom -> Exp e
        ExpFun :: String -> Exp e
        ExpMap :: e -> Exp e
        ExpFold :: e -> Exp e
        deriving (Functor)
    
    data Def e = Fundef Type String Type e deriving Show
    
    data Prgm e = Prgm [Def e] e deriving Show
    
    showDef :: Expression e => Def e -> [String]
    showDef (Fundef t1 f t2 e) =
        [ f
        , "  : " ++ show t1 ++ " |- " ++ show t2
        , "  = " ++ prettyPrint e ++ ";"
        ]
    
    showPrgm :: Expression e => Prgm e -> [String]
    showPrgm (Prgm ds e) = (concatMap ((++[""]) . showDef) ds) ++ [prettyPrint e]
    
    newtype Expr = Expr { unExpr :: Exp Expr }
    
    instance Show Expr where
        show = prettyPrint
    
    newtype Parser e = Parser { runParser :: String -> [(e, String)] } deriving Functor
    
    instance Applicative Parser where
        pure x = Parser (\w -> [(x, w)])
        Parser p1 <*> Parser p2 =
            Parser (\ w -> do
                (f, w1) <- p1 w
                (x, w2) <- p2 w1
                return (f x, w2)
            )
    
    instance Monad Parser where
        return = pure
        Parser p1 >>= f =
            Parser (\w -> do 
                (x, w1) <- p1 w
                runParser (f x) w1
                )
    
    instance Alternative Parser where
        Parser p1 <|> Parser p2 = Parser (\w -> take 1 $ p1 w ++ p2 w)
        empty = Parser (const [])
    
    -- end HL
    
    -- begin LL
    
    data Lvalue 
        = Cident String 
        | Cindex Lvalue Lvalue
        | Caccess Lvalue Int
        deriving (Show, Eq)
    
    emitLvalue :: Lvalue -> String
    emitLvalue x =
        case x of
            Cident x0 -> x0
            Cindex x0 i -> emitLvalue x0 ++ "[" ++ emitLvalue i ++ "]"
            Caccess x0 i -> emitLvalue x0 ++ ".x" ++ show i
    
    data Ctype = CtFloat | CtInt | CtDef String Ctypedef deriving Eq
    
    ctypeShortName :: Ctype -> String
    ctypeShortName ct =
        case ct of
            CtFloat -> "f"
            CtInt -> "i"
            CtDef x _ -> x
    
    data Cexp where
        Cvar :: Lvalue -> Cexp
        Cfloat :: Float -> Cexp
        Cint :: Int -> Cexp
        CbinOp :: String -> Lvalue -> Lvalue -> Cexp
        Cmax :: Lvalue -> Lvalue -> Cexp
        Ccall :: Lvalue -> Lvalue -> Cexp
        CstructInitializer :: [Lvalue] -> Cexp
        CunionInitializer :: Int -> Lvalue -> Cexp
        Cmalloc :: Lvalue -> Cexp
        CsizeOf :: Ctype -> Cexp
        Cdereference :: Lvalue -> Cexp
    
    data Cstmt where
        Cass :: Ctype -> String -> Cexp -> Cstmt
        Cdecl :: Ctype -> String -> Cstmt
        Cass0 :: Lvalue -> Cexp -> Cstmt
        Creturn :: Lvalue -> Cstmt
        Cswitch :: Lvalue -> [[Cstmt]] -> Cstmt
        Cwhile :: Lvalue -> [Cstmt] -> Cstmt
        Csyscall :: String -> [Lvalue] -> Cstmt
        Ccomment :: String -> Cstmt
    
    newtype Ccodegen r = Ccodegen { runCcodegen :: Int -> [Cdefinition] -> ([Cdefinition], [Cstmt], r, Int) } deriving Functor
    
    data Ctypedef
        = CtdefUnion [Ctype]
        | CtdefStruct [Ctype]
        | CtdefFun Ctype Ctype
        | CtdefPtr Ctype
        deriving Eq
    
    data Cdefinition 
        = Cfunction Ctype String Ctype String [Cstmt]
        | Ctypedef String Ctypedef
        | Ctemplate String (Type -> Type -> Ccodegen ())
        | CfunctionDecleration Ctype String Ctype
    
    data Cprgm = Cprgm [Cdefinition] [Cstmt]
    
    instance Applicative Ccodegen where
        pure x = Ccodegen (\ _ _ -> ([], [], x, 0))
        c1 <*> c2 =
            Ccodegen (\i ds ->
                let (ds1, ss1, f, i1) = runCcodegen c1 i ds
                    (ds2, ss2, x, i2) = runCcodegen c2 (i + i1) (ds ++ ds1)
                in (ds1 ++ ds2, ss1 ++ ss2, f x, i1 + i2)
            )
    
    instance Monad Ccodegen where
        return = pure
        c1 >>= f =
            Ccodegen (\i ds ->
                let (ds1, ss1, x, i1) = runCcodegen c1 i ds
                    (ds2, ss2, y, i2) = runCcodegen (f x) (i + i1) (ds ++ ds1)
                in (ds1 ++ ds2, ss1 ++ ss2, y, i1 + i2)
                )
    
    instance MonadFail Ccodegen where
        fail x = error ("cannot compile: " ++ x)
    
    cfreshIdent :: String -> Ccodegen String
    cfreshIdent x = Ccodegen (\ i _ -> ([], [], (x ++ show i), 1))
    
    cstmt :: Cstmt -> Ccodegen ()
    cstmt c = Ccodegen (\ _ _ -> ([], [c], (), 0))
    
    cdefinition :: Cdefinition -> Ccodegen ()
    cdefinition d = Ccodegen (\ _ _ -> ([d], [], (), 0))
    
    cblock :: Ccodegen a -> Ccodegen [Cstmt]
    cblock c = 
        Ccodegen (\i ds ->
            let (ds1, ss1, _, i0) = runCcodegen c i ds
            in (ds1, [], ss1, i0)
            )
    
    ccLookupTypedef :: Ctypedef -> Ccodegen (Maybe Ctype)
    ccLookupTypedef d =
        Ccodegen (\ _ ds -> ([], [], look ds, 0))
        where
            look [] = Nothing
            look (Ctypedef i d2:_) | d == d2 = Just (CtDef i d2)
            look (_:ds0) = look ds0
    
    ccLookupFunction :: String -> Ccodegen (Bool)
    ccLookupFunction f =
        Ccodegen (\ _ ds -> ([], [], look ds, 0))
        where
            look [] = False
            look (Cfunction _ g _ _ _:_) | f == g = True
            look (CfunctionDecleration _ g _:_) | f == g = True
            look (_:ds0) = look ds0
    
    ccLookupTemplate :: String -> Ccodegen (Maybe (Type -> Type -> Ccodegen()))
    ccLookupTemplate f = 
        Ccodegen (\ _ ds -> ([], [], look ds, 0))
        where
            look [] = Nothing
            look (Ctemplate g t:_) | f == g = Just t
            look (_:ds0) = look ds0
    
    ccTemplate :: String -> (Type -> Type -> Ccodegen ()) -> Ccodegen ()
    ccTemplate f t = cdefinition (Ctemplate f t)
    
    ccInstantiateTemplate :: Type -> String -> Type -> Ccodegen String
    ccInstantiateTemplate t1 f t2 = do
        ct1 <- ctype t1
        ct2 <- ctype t2
        let f' = cfunctionInstanceName ct1 f ct2
        b <- ccLookupFunction f'
        if b 
        then return f'
        else do
            Just t <- ccLookupTemplate f
            t t1 t2
            return f'
    
    cfunctionInstanceName :: Ctype -> String -> Ctype -> String
    cfunctionInstanceName ct1 f ct2 = f ++ "_" ++ ctypeShortName ct1 ++ "_" ++ ctypeShortName ct2
    
    -- end LL
    
    newtype TypeInf a = TypeInf { runTypeInf :: Int -> (a, [(Type, Type)], Int) } deriving Functor
    
    instance Applicative TypeInf where
        pure x = TypeInf (\_ -> (x, [], 0))
        t1 <*> t2 =
            TypeInf (\i ->
                let (f, g1, i1) = runTypeInf t1 i
                    (x, g2, i2) = runTypeInf t2 (i + i1)
                in (f x, (g1 ++ g2), i1 + i2)
            )
    
    instance Monad TypeInf where
        return = pure
        t1 >>= f =
            TypeInf (\i ->
                let (x, g1, i1) = runTypeInf t1 i
                    (y, g2, i2) = runTypeInf (f x) (i + i1)
                in (y, (g1 ++ g2), i1 + i2)
            )
    
    addConstraint :: Type -> Type -> TypeInf ()
    addConstraint t1 t2 = 
        if t1 == t2
        then return ()
        else TypeInf (\_ -> ((), [(t1, t2)], 0))
    
    freshTvar :: TypeInf Type
    freshTvar = TypeInf (\i -> (Tvar i, [], 1))
    
    class TypeSubst t where
        typeSubstitution :: Int -> Type -> t -> t
        forallGeneralization :: t -> t
        occursCheck :: Int -> t -> Bool
        typeVars :: t -> [Int]
    
        substituteMgu :: [(Type, Type)] -> t -> t
        substituteMgu g x =
            let g' = map (\ (t1, t2) -> 
                    case t1 of
                        Tvar i -> (i, t2)
                        _ -> error $ show t1) g
            in foldl (\ acc (i, t) -> typeSubstitution i t acc) x g'
            
        substituteMgu' :: [(Int, Type)] -> t -> t
        substituteMgu' g x = substituteMgu (map (\ (i, t) -> (Tvar i, t)) g) x
    
        substituteMgu'' :: [(Int, Int)] -> t -> t
        substituteMgu'' g x = substituteMgu (map (\ (i, j) -> (Tvar i, Tvar j)) g) x
    
        instantiate :: t -> TypeInf t
        instantiate t = do
            let tvars = typeVars t
            tvars' <- mapM (const freshTvar) tvars
            return $ substituteMgu' (zip tvars tvars') t
    
        monomorphise :: t -> t
        monomorphise t =
            let tvars = sort $ typeVars t
            in substituteMgu' (tvars `zip` repeat Tunit) t
    
        forallInstantiation :: t -> t
        forallInstantiation t =
            let tvars = sort $ typeVars t
            in substituteMgu' (map (\ i -> (i, TvarExistential i)) tvars) t
    
    instance TypeSubst Type where
        typeSubstitution i t0 t =
            let sub = typeSubstitution i t0
            in
            case t of
                Tvar j  | i == j -> t0
                        | otherwise -> Tvar j
                TvarExistential j -> TvarExistential j
                Tpair t1 t2 -> Tpair (sub t1) (sub t2)
                Tunit -> Tunit
                Tfloat -> Tfloat
                Teither t1 t2 -> Teither (sub t1) (sub t2)
                Tempty -> Tempty
                Tarray t1 -> Tarray (sub t1)
    
        forallGeneralization t =
            let gen = forallGeneralization
            in
            case t of
                TvarExistential i -> Tvar i
                Tpair t1 t2 -> Tpair (gen t1) (gen t2)
                Teither t1 t2 -> Teither (gen t1) (gen t2)
                Tarray t1 -> Tarray (gen t1)
                _ -> t
    
        typeVars t =
            case t of
                Tvar j -> [j]
                TvarExistential _ -> []
                Tpair t1 t2 -> typeVars t1 `union` typeVars t2
                Tunit -> []
                Tfloat -> []
                Teither t1 t2 -> typeVars t1 `union` typeVars t2
                Tempty -> []
                Tarray t1 -> typeVars t1
    
        occursCheck i t =
            let occ = occursCheck i
            in
            case t of
                Tvar j  | i == j -> True
                        | otherwise -> False
                TvarExistential _ -> False
                Tpair t1 t2 -> occ t1 || occ t2
                Tunit -> False
                Tfloat -> False
                Teither t1 t2 -> occ t1 || occ t2
                Tempty -> False
                Tarray t1 -> occ t1
    
    instance TypeSubst TypedExp where
        typeSubstitution i t0 (TypedExp t1 e t2) = TypedExp (typeSubstitution i t0 t1) (typeSubstitution i t0 e) (typeSubstitution i t0 t2)
        forallGeneralization (TypedExp t1 e t2) = TypedExp (forallGeneralization t1) (forallGeneralization e) (forallGeneralization t2)
        typeVars (TypedExp t1 _ t2) = typeVars t1 `union` typeVars t2
        occursCheck i (TypedExp t1 e t2) = occursCheck i t1 || occursCheck i e || occursCheck i t2
    
    instance TypeSubst e => TypeSubst (Def e) where
        typeSubstitution i t0 d = 
            case d of
                Fundef t1 f t2 e -> Fundef (typeSubstitution i t0 t1) f (typeSubstitution i t0 t2) (typeSubstitution i t0 e)
        forallGeneralization d =
            case d of
                Fundef t1 f t2 e -> Fundef (forallGeneralization t1) f (forallGeneralization t2) (forallGeneralization e)
        typeVars d =
            case d of
                Fundef t1 _ t2 e -> typeVars t1 `union` typeVars t2 `union` typeVars e
        occursCheck i d =
            case d of
                Fundef t1 _ t2 e -> occursCheck i t1 || occursCheck i t2 || occursCheck i e
    
    instance TypeSubst e => TypeSubst (Prgm e) where
        typeSubstitution i t0 (Prgm ds e) = Prgm ds (typeSubstitution i t0 e)
        forallGeneralization (Prgm ds e) = Prgm ds (forallGeneralization e)
        typeVars (Prgm _ e) = typeVars e
        occursCheck i (Prgm _ e) = occursCheck i e
    
    instance TypeSubst e => TypeSubst (Exp e) where
        typeSubstitution i t0 e = 
            let sub = typeSubstitution i t0
            in
            case e of
                ExpApp e1 e2 -> ExpApp (sub e1) (sub e2)
                ExpPair e1 e2 -> ExpPair (sub e1) (sub e2)
                ExpEither e1 e2 -> ExpEither (sub e1) (sub e2)
                ExpAxiom e0 -> ExpAxiom e0
                ExpFun f -> ExpFun f
                ExpMap e1 -> ExpMap (sub e1)
                ExpFold e1 -> ExpFold (sub e1)
    
        forallGeneralization e =
            let gen = forallGeneralization
            in
            case e of
                ExpApp e1 e2 -> ExpApp (gen e1) (gen e2)
                ExpPair e1 e2 -> ExpPair (gen e1) (gen e2)
                ExpEither e1 e2 -> ExpEither (gen e1) (gen e2)
                ExpAxiom e0 -> ExpAxiom e0
                ExpFun f -> ExpFun f
                ExpMap e0 -> ExpMap (gen e0)
                ExpFold e0 -> ExpFold (gen e0)
    
        typeVars e = 
            case e of
                ExpApp e1 e2 -> typeVars e1 `union` typeVars e2
                ExpPair e1 e2 -> typeVars e1 `union` typeVars e2
                ExpEither e1 e2 -> typeVars e1 `union` typeVars e2
                ExpAxiom _ -> []
                ExpFun _ -> []
                ExpMap e1 -> typeVars e1
                ExpFold e1 -> typeVars e1
    
        occursCheck i e = 
            let occ = occursCheck i
            in
            case e of
                ExpApp e1 e2 -> occ e1 || occ e2
                ExpPair e1 e2 -> occ e1 || occ e2
                ExpEither e1 e2 -> occ e1 || occ e2
                ExpAxiom _ -> False
                ExpFun _ -> False
                ExpMap e1 -> occ e1
                ExpFold e1 -> occ e1
    
    instance TypeSubst t => TypeSubst (t, t) where
        typeSubstitution i t0 (t1, t2) = (typeSubstitution i t0 t1, typeSubstitution i t0 t2)
        forallGeneralization (t1, t2) = (forallGeneralization t1, forallGeneralization t2)
        typeVars (t1, t2) = typeVars t1 `union` typeVars t2
        occursCheck i (t1, t2) = occursCheck i t1 || occursCheck i t2
    
    instance TypeSubst t => TypeSubst [t] where
        typeSubstitution i t0 ts = map (typeSubstitution i t0) ts
        forallGeneralization ts = map forallGeneralization ts
        typeVars ts = foldl union [] (map typeVars ts)
        occursCheck i ts = any (occursCheck i) ts
    
    showConstraints :: (Show a, Show b) => [(a, b)] -> String
    showConstraints cs = intercalate "\n" (map (\ (x, y) -> show x ++ " = " ++ show y) cs)
    
    solveConstraints :: String -> [(Type, Type)] -> [(Type, Type)]
    solveConstraints name cs =
        case runWriter (runExceptT (solveConstraintsE cs)) of
            (Left err, steps) -> error (name ++ ": " ++ err ++ "\nSTEPS:\n" ++ steps)
            (Right x, _) -> x
    
    solveConstraintsE :: [(Type, Type)] -> ExceptT String (Writer String) [(Type, Type)]
    solveConstraintsE cs =
        lift (tell (showConstraints cs ++ "\n")) >>
        case cs of
            (t1, t2) : cs0 | t1 == t2 -> solveConstraintsE cs0 -- delete
            (TvarExistential i, TvarExistential j) : cs0 | i == j -> solveConstraintsE cs0 -- decompose-existential
            (Tpair t11 t12, Tpair t21 t22) : cs0 -> solveConstraintsE $ (t11, t21) : (t12, t22) : cs0 -- decompose-pair
            (Teither t11 t12, Teither t21 t22) : cs0 -> solveConstraintsE $ (t11, t21) : (t12, t22) : cs0 -- decompose-plus
            (Tarray t1, Tarray t2) : cs0 -> solveConstraintsE $ (t1, t2) : cs0 -- decompose-array
            (t1, Tvar i) : cs0 | not (isTvar t1) -> solveConstraintsE $ (Tvar i, t1) : cs0 -- swap
            (Tvar i, t2) : cs0
                | occursCheck i t2 -> throwError $ "OCCURS CHECK FAILED " ++ show (head cs)  -- check
                | occursCheck i cs0 -> solveConstraintsE $ typeSubstitution i t2 cs0 ++ [(Tvar i, t2)] -- eliminate
                | otherwise -> (:) (Tvar i, t2) <$> solveConstraintsE cs0 -- recurse
            [] -> return [] -- done
            _ -> throwError $ "TYPE CONFLICT: " ++ show (head cs) -- conflict
    
    data TypedExp = TypedExp Type (Exp TypedExp) Type
    
    data TypeContext = TypeContext [(String, (Type, Type))]
    
    lookupFun :: String -> TypeContext -> (Type, Type)
    lookupFun f (TypeContext g) =
        case lookup f g of
            Nothing -> error $ "UNBOUND FUNCTION: " ++ f
            Just f0 -> f0
    
    {-
    G ::= [ident => t -> t, ...]
    
    +-------------+
    |G, t |- e : t|
    +-------------+
    
    G, t |- e1 : t1      G, t |- e2 : t2
    ------------------------------------
    G, t |- e1 , e2 : t1 , t2
    
    ------------------------ ( G[ident] = t1 -> t2 )
       G, t1 |- ident : t2
    
    -------------------
    t1 , t2 |- fst : t1
    
    t1 |- e1 : t    t2 |- e2 : t
    -----------------------------
    t1 | t2 |- e1 | e2 : t
    
    ----------------------
    t1 |- inl e1 : t1 | t2
    
    t1 |- e1 : t2   t |- e2 : t1
    ----------------------------
    t |- e1 e2 : t2
    
    ----------
    t |- x : t
    
    ------------------------------------------
    (t1 | t2) , t3 |- dist : t1 , t3 | t2 , t3
    
    ---------------------------
    float , float |- + : float
    
    
    array rules:
    -------------------- // malloc
    float |- & : [float]
    
    -------------------------
    a * ... * a |- list : [a]
    
    ---------------- // array.x0
    [t] |- # : float
    
    ------------------- // *array.x1
    [t] |- head : a + 1
    
    ------------------------- // array.x0 -= n
    [t] * float |- take : [t]
    
    ------------------------- // array.x0 -= n; array.x1 += n
    [t] * float |- drop : [t]
    
    ------------- // empty array
    t1 |- [] : [t2]
    
    --------------------- // array1.x0 + array2.x0; memcpy array1.x1; memcpy array2.x1
    [t] * [t] |- ++ : [t]
    
    ---------------------------- // EXPENSIVE :(.... maybe use SoA and special constant arrays repr? Or full-blown SNESL? Or Forth stack? Or believe in hoisting?
    [t1] * t2 |- str : [t1 * t2]
    
          t1 |- e0 : t2 
    ------------------------- // malloc; for-loop with write (avoids sum type)
        [t1] |- [e0] : [t2]
    
        t1 * t2 |- e0 : t1 
    -------------------------------- // for-loop accumulator
      t1 * [t2] |- <e0> : t1
    
    
    stream rules:
    
    a |- stdin : {float}
    
    float |- & : {float}
    
          t1 |- e0 : t2 
    ------------------------- // malloc; for-loop with write (avoids sum type)
        {t1} |- {e0} : {t2}
    
        t1 * t2 |- e0 : t1 
    -------------------------------- // for-loop accumulator
      t1 * {t2} |- <e0> : t1
    
    -}
    typeInferenceAxiom :: Type -> ExpAxiom -> TypeInf Type
    typeInferenceAxiom tx e =
        case e of
            ExpNum _ -> return Tfloat
            ExpUnit -> return Tunit
            ExpEmpty -> do
                addConstraint tx Tempty
                freshTvar
            ExpFst ->
                case tx of 
                    Tpair t1 _ -> return t1
                    _ -> do
                        t1 <- freshTvar
                        t2 <- freshTvar
                        addConstraint tx (Tpair t1 t2)
                        return t1
            ExpSnd ->
                case tx of
                    Tpair _ t2 -> return t2
                    _ -> do
                        t1 <- freshTvar
                        t2 <- freshTvar
                        addConstraint tx (Tpair t1 t2)
                        return t2
            ExpInl -> do
                t2 <- freshTvar
                return (Teither tx t2)
            ExpInr -> do
                t1 <- freshTvar
                return (Teither t1 tx)
            ExpDist -> do
                t1 <- freshTvar
                t2 <- freshTvar
                t3 <- freshTvar
                addConstraint tx (Tpair (Teither t1 t2) t3)
                return (Teither (Tpair t1 t3) (Tpair t2 t3))
            ExpMinus -> do
                addConstraint tx (Tpair Tfloat Tfloat)
                return Tfloat
            ExpPlus -> do
                addConstraint tx (Tpair Tfloat Tfloat)
                return Tfloat
            ExpTimes -> do
                addConstraint tx (Tpair Tfloat Tfloat)
                return Tfloat
            ExpEquals -> do
                addConstraint tx (Tpair Tfloat Tfloat)
                return (Teither Tunit Tunit)
            ExpVar -> return tx
            ExpIota -> do
                addConstraint tx Tfloat
                return (Tarray Tfloat)
            ExpMkList n -> do
                t1 <- freshTvar 
                let t2 = mkTupleType (replicate n t1)
                addConstraint tx t2
                return (Tarray t1)
            ExpLength -> do
                t1 <- freshTvar
                addConstraint tx (Tarray t1)
                return Tfloat
            ExpHead -> do
                t1 <- freshTvar
                addConstraint tx (Tarray t1)
                return (Teither t1 Tunit)
            ExpTake -> do
                t1 <- freshTvar
                addConstraint tx (Tpair (Tarray t1) Tfloat)
                return (Tarray t1)
            ExpDrop -> do
                t1 <- freshTvar
                addConstraint tx (Tpair (Tarray t1) Tfloat)
                return (Tarray t1)
            ExpEmptyArray -> do
                t1 <- freshTvar
                return (Tarray t1)
            ExpAppend -> do
                t1 <- freshTvar
                addConstraint tx (Tpair (Tarray t1) (Tarray t1))
                return (Tarray t1)
            ExpStr -> do
                t1 <- freshTvar
                t2 <- freshTvar
                addConstraint tx (Tpair (Tarray t1) t2)
                return (Tarray (Tpair t1 t2))
    
    typeInferenceExp :: Expression e => TypeContext -> Type -> Exp e -> TypeInf TypedExp
    typeInferenceExp g tx e =
        case e of
            ExpApp e1 e2 -> do
                e2' <- typeInference g tx e2
                e1' <- typeInference g (outType e2') e1
                return $ TypedExp tx (ExpApp e1' e2') (outType e1')
            ExpPair e1 e2 -> do
                e1' <- typeInference g tx e1
                e2' <- typeInference g tx e2
                return $ TypedExp tx (ExpPair e1' e2') (Tpair (outType e1') (outType e2'))
            ExpEither e1 e2 -> do
                (t1, t2) <- case tx of
                    Teither t1 t2 -> return (t1, t2)
                    _ -> do
                        t1 <- freshTvar
                        t2 <- freshTvar
                        addConstraint tx (Teither t1 t2)
                        return (t1, t2)
                e1' <- typeInference g t1 e1
                e2' <- typeInference g t2 e2
                addConstraint (outType e1') (outType e2')
                return $ TypedExp (Teither t1 t2) (ExpEither e1' e2') (outType e1')
            ExpAxiom e0 -> do 
                t0 <- typeInferenceAxiom tx e0
                return $ TypedExp tx (ExpAxiom e0) t0
            ExpFun f -> do
                let (t1, t2) = lookupFun f g
                (t1', t2') <- instantiate (t1, t2)
                addConstraint tx t1'
                return $ TypedExp tx (ExpFun f) t2'
            ExpMap e0 -> do
                t1 <- case tx of
                    Tarray t1 -> return t1
                    _ -> do
                        t1 <- freshTvar
                        addConstraint tx (Tarray t1)
                        return t1
                e0' <- typeInference g t1 e0
                return $ TypedExp (Tarray t1) (ExpMap e0') (Tarray (outType e0'))
            ExpFold e0 -> do
                (t1, t2) <- case tx of
                    (Tpair t1 (Tarray t2)) -> return (t1, t2)
                    _ -> do
                        t1 <- freshTvar
                        t2 <- freshTvar
                        addConstraint tx (Tpair t1 (Tarray t2))
                        return (t1, t2)
                e0' <- typeInference g (Tpair t1 t2) e0
                addConstraint (outType e0') t1
                return $ TypedExp (Tpair t1 (Tarray t2)) (ExpFold e0') t1
    
    typeInferenceFundef :: Expression e => TypeContext -> Def e -> Def TypedExp
    typeInferenceFundef g d =
        case d of
            Fundef t1 f t2 e0 ->
                let tvars = typeVars (t1, t2)
                    sub = map (\t -> (t, TvarExistential t)) tvars 
                    chk = do
                        e0' <- typeInference g (substituteMgu' sub t1) e0
                        addConstraint (substituteMgu' sub t2) (outType e0')
                        return e0'
                    (e0'', w, _) = runTypeInf chk (foldl max 0 tvars + 1)
                    isConsistent [] = True
                    isConsistent (_ : cs0) = isConsistent cs0
                    mgu = solveConstraints (show t1 ++ " |- " ++ f ++ " : " ++ show t2) w
                in 
                if isConsistent mgu
                then forallGeneralization $ Fundef t1 f t2 (monomorphise $ substituteMgu mgu e0'')
                else error ("TYPE DEFINITION INCORRECT FOR " ++ f ++ (show $ mgu))
    
    typeInferencePrgm :: Expression e => Prgm e -> TypeInf (Prgm TypedExp)
    typeInferencePrgm (Prgm ds e) =
        let g = TypeContext $ map (\ (Fundef t1 f t2 _) -> (f, (t1, t2))) ds
        in do
            let ds' = map (typeInferenceFundef g) ds
            e' <- typeInference g Tunit e
            return $ Prgm ds' e'
    
    data ValueContext m = ValueContext [(String, Value -> m Value)]
    
    lookupFunValue :: String -> ValueContext m -> Value -> m Value
    lookupFunValue f (ValueContext g) =
        case lookup f g of
            Nothing -> error $ "UNBOUND FUNCTION: " ++ f
            Just h -> h
    
    {-
    +-----------+
    |v |- e -> v|
    +-----------+
    
    v |- e1 -> v1   v |- e2 -> v2
    -------------------------------
    v |- e1 , e2 -> v1 , v2
    
     v1 |- e1 -> v
    ----------------------
    inl v1 |- e1 | e2 -> v
    
    -------------------
    v1 , v2 |- fst -> v1
    
    -----------------
    v |- inl -> inl v
    
    -----------
    v |- x -> v
    
    -------------------
    v |- float -> float
    
    ------------------------------------
    inl v1 , v2 |- dist -> inl (v1 , v2)
    
    ------------------------------------
    inr v1 , v2 |- dist -> inr (v1 , v2)
    
    v2 |- e1 -> v1           v |- e2 -> v2
    --------------------------------------
    v |- e1 e2 -> v1
    -}
    
    interpretAxiom :: MonadFail m => Value -> ExpAxiom -> m Value
    interpretAxiom vx e =
        case e of
            ExpNum x -> return $ Vfloat x
            ExpUnit -> return $ Vunit
            ExpEmpty -> undefined
            ExpFst -> case vx of
                Vpair v1 _ -> return v1
                _ -> undefined
            ExpSnd -> case vx of
                Vpair _ v2 -> return v2
                _ -> undefined
            ExpInl -> return $ Vinl vx
            ExpInr -> return $ Vinr vx
            ExpDist -> case vx of
                Vpair (Vinl v1) v2 -> return $ Vinl (Vpair v1 v2)
                Vpair (Vinr v1) v2 -> return $ Vinr (Vpair v1 v2)
                _ -> undefined
            ExpMinus -> case vx of
                Vpair (Vfloat x) (Vfloat y) -> return $ Vfloat (x - y)
                _ -> undefined
            ExpPlus -> case vx of
                Vpair (Vfloat x) (Vfloat y) -> return $ Vfloat (x + y)
                _ -> undefined
            ExpTimes -> case vx of
                Vpair (Vfloat x) (Vfloat y) -> return $ Vfloat (x * y)
                _ -> undefined
            ExpEquals -> case vx of
                Vpair (Vfloat x) (Vfloat y) -> return $ if x == y then Vinl Vunit else Vinr Vunit
                _ -> undefined
            ExpVar -> return vx
            ExpIota -> case vx of
                Vfloat x -> return $ Varray (map Vfloat [0..x])
                _ -> undefined
            ExpMkList 0 -> return $ Varray []
            ExpMkList 1 -> return $ Varray [vx]
            ExpMkList n -> case vx of
                Vpair v1 v2 -> do
                    Varray vs <- interpretAxiom v2 (ExpMkList (n - 1))
                    return $ Varray (v1 : vs)
                _ -> undefined
            ExpLength -> case vx of
                Varray vs -> return $ Vfloat (fromIntegral $ length vs)
                _ -> undefined
            ExpHead -> case vx of
                Varray vs -> return $ case vs of
                    [] -> Vinr Vunit
                    x:_ -> Vinl x
                _ -> undefined
            ExpTake -> case vx of
                Vpair (Varray vs) (Vfloat x) -> return $ Varray (take (floor x) vs)
                _ -> undefined
            ExpDrop -> case vx of
                Vpair (Varray vs) (Vfloat x) -> return $ Varray (drop (floor x) vs)
                _ -> undefined
            ExpEmptyArray -> return $ Varray []
            ExpAppend -> case vx of
                Vpair (Varray vs1) (Varray vs2) -> return $ Varray (vs1 ++ vs2)
                _ -> undefined
            ExpStr -> case vx of
                Vpair (Varray vs) v -> return $ Varray (map (\ v0 -> Vpair v0 v) vs)
                _ -> undefined
    
    interpretExp :: MonadFail m => Expression e => ValueContext m -> Value -> Exp e -> m Value
    interpretExp g vx e =
        case e of
            ExpApp e1 e2 -> do
                v2 <- interpret g vx e2
                v1 <- interpret g v2 e1
                return v1
            ExpPair e1 e2 -> do
                v1 <- interpret g vx e1
                v2 <- interpret g vx e2
                return $ Vpair v1 v2
            ExpEither e1 e2 ->
                case vx of
                    Vinl v -> interpret g v e1
                    Vinr v -> interpret g v e2
                    _ -> undefined
            ExpAxiom e0 -> interpretAxiom vx e0
            ExpFun f -> lookupFunValue f g vx
            ExpMap e0 -> do
                case vx of
                    Varray vs -> Varray <$> mapM (\v0 -> interpret g v0 e0) vs
                    _ -> undefined
            ExpFold e0 -> do
                let fo v1 [] = return v1
                    fo v1 (v2:vs2) = do
                        v1' <- interpret g (Vpair v1 v2) e0
                        fo v1' vs2
                case vx of
                    Vpair v1 (Varray vs2) -> fo v1 vs2
                    _ -> undefined
    
    prettyPrintAxiom :: ExpAxiom -> String
    prettyPrintAxiom e =
        case e of
            ExpNum x -> show x
            ExpUnit -> "unit"
            ExpEmpty -> "empty"
            ExpFst -> "fst"
            ExpSnd -> "snd"
            ExpInl -> "inl"
            ExpInr -> "inr"
            ExpDist -> "dist"
            ExpMinus -> "-"
            ExpPlus -> "+"
            ExpTimes -> "*"
            ExpEquals -> "=="
            ExpVar -> "x"
            ExpIota -> "&"
            ExpMkList _ -> "list"
            ExpLength -> "#"
            ExpHead -> "head"
            ExpTake -> "take"
            ExpDrop -> "drop"
            ExpEmptyArray -> "[]"
            ExpAppend -> "append"
            ExpStr -> "str"
    
    prettyPrintExp :: Expression e => Exp e -> String
    prettyPrintExp e =
        case e of
            ExpApp e1 e2 -> prettyPrint e1 ++ " " ++ prettyPrint e2
            ExpPair e1 e2 -> "(" ++ prettyPrint e1 ++ ", " ++ prettyPrint e2 ++ ")"
            ExpEither e1 e2 -> "(" ++ prettyPrint e1 ++ " | " ++ prettyPrint e2 ++ ")"
            ExpAxiom e0 -> prettyPrintAxiom e0
            ExpFun f -> f
            ExpMap e0 -> "[" ++ prettyPrint e0 ++ "]"
            ExpFold e0 -> "<" ++ prettyPrint e0 ++ ">"
    
    outTypeMain :: Typed e => Prgm e -> Type
    outTypeMain (Prgm _ e) = outType e
    
    run :: String -> IO ()
    run w = 
        let p = parse w
            p' = infer p
            t = outTypeMain p'
            c = compilePrgm p'
            src = emit t c
        in do
            putStrLn $ src 
            tdir <- getTemporaryDirectory
            let dir = tdir </> "alang"
            createDirectoryIfMissing False dir
            let fp = dir </> "run.c"
            h <- openFile fp WriteMode
            hPutStr h src
            hClose h
            -- putStrLn $ fp
            let fpOut = dropExtension fp ++ ".exe"
                com = "gcc -O3 -o " ++ fpOut ++ " " ++ fp
            system com
    
            
            let fpOutAsm = dropExtension fp ++ ".s"
                comAsm = "gcc -Ofast -S -o " ++ fpOutAsm ++ " " ++ fp
            system comAsm
            
            putStrLn ""
            mapM_ putStrLn (showPrgm p')
            -- putStrLn com
            putStrLn ""
            code <- system fpOut
            case code of
                ExitSuccess -> putStrLn ""
                ExitFailure n -> putStrLn $ "\nexit failure " ++ show n
            return ()
    
    runExample :: String -> IO ()
    runExample fp = do
        file <- readFile $ "src" </> "examples" </> fp <.> "alang"
        run file
    
    class Ctyped t where
        ctype :: t -> Ccodegen Ctype
    
        ctTuple :: [t] -> Ccodegen Ctype
        ctTuple ts = do 
            cts <- mapM ctype ts
            ccTypedef (CtdefStruct cts)
    
        ctUnion :: [t] -> Ccodegen Ctype
        ctUnion ts = do
            cts <- mapM ctype ts
            ccTypedef (CtdefUnion cts)
    
        ctVariant :: [t] -> Ccodegen Ctype
        ctVariant ts = do
            ct <- ctUnion ts
            ctTuple [CtInt, ct]
    
        ctPtr :: t -> Ccodegen Ctype
        ctPtr t = do
            ct <- ctype t
            ccTypedef (CtdefPtr ct)
    
        ctArray :: t -> Ccodegen Ctype
        ctArray t = do
            ct <- ctPtr t
            ctTuple [CtInt, ct]
    
        cassign :: t -> Cexp -> Ccodegen Lvalue
        cassign t e = do
            ct <- ctype t
            x <- cfreshIdent "x"
            cstmt (Cass ct x e)
            return (Cident x)
    
        cdeclare :: t -> Ccodegen Lvalue
        cdeclare t = do
            ct <- ctype t
            x <- cfreshIdent "x"
            cstmt (Cdecl ct x)
            return (Cident x)
    
        ccall :: t -> Lvalue -> Lvalue -> Ccodegen Lvalue
        ccall t f x = cassign t (Ccall f x)
    
        cbinop :: String -> t -> Lvalue -> Lvalue -> Ccodegen Lvalue
        cbinop o t x1 x2 = cassign t (CbinOp o x1 x2)
    
        cmax :: t -> Lvalue -> Lvalue -> Ccodegen Lvalue
        cmax t x1 x2 = cassign t (Cmax x1 x2)
    
        csizeof :: t -> Ccodegen Lvalue
        csizeof t = do
            ct <- ctype t
            cassign CtInt (CsizeOf ct)
    
        cmemcpy :: t -> Lvalue -> Lvalue -> Lvalue -> Ccodegen ()
        cmemcpy t x1 x2 n = do
            sz <- csizeof t
            n' <- cbinop "*" CtInt n sz
            cstmt (Csyscall "memcpy" [x1, x2, n'])
    
        ctuple :: [t] -> [Lvalue] -> Ccodegen Lvalue
        ctuple ts xs = do
            ct <- ctTuple ts
            cassign ct (CstructInitializer xs)
        
        cpair :: t -> t -> Lvalue -> Lvalue -> Ccodegen Lvalue
        cpair t1 t2 x1 x2 = ctuple [t1, t2] [x1, x2]
    
        cvariant :: [t] -> Int -> Lvalue -> Ccodegen Lvalue
        cvariant ts i x = do
            ct <- ctVariant ts
            cassign ct (CunionInitializer i x)
    
        cinl :: t -> t -> Lvalue -> Ccodegen Lvalue
        cinl t1 t2 x = cvariant [t1, t2] 0 x
    
        cinr :: t -> t -> Lvalue -> Ccodegen Lvalue
        cinr t1 t2 x = cvariant [t1, t2] 1 x
    
        ceither :: t -> Lvalue -> (Lvalue -> Ccodegen Lvalue) -> (Lvalue -> Ccodegen Lvalue) -> Ccodegen Lvalue
        ceither t x f g = do
            xret <- cdeclare t
            ss1 <- cblock $ do
                x1 <- f (cfst $ csnd x)
                cassign0 xret (Cvar x1)
            ss2 <- cblock $ do
                x2 <- g (csnd $ csnd x)
                cassign0 xret (Cvar x2)
            cstmt (Cswitch (cfst x) [ss1, ss2])
            return xret
    
        cnewArray :: t -> Lvalue -> Ccodegen Lvalue
        cnewArray t x = do
            x0 <- csizeof t
            x1 <- cbinop "*" CtInt x x0
            ct <- ctPtr t
            ptr <- cassign ct (Cmalloc x1)
            cpair CtInt ct x ptr
    
        cmkList :: Int -> t -> Lvalue -> Ccodegen Lvalue
        cmkList n t x = do
            let xs = cmatchTuple n x
            xl <- cint n
            x0 <- cnewArray t xl 
            mapM (\ (x1, i) -> do
                xi <- cint i
                carrayWrite x0 xi (Cvar x1)
                ) (xs `zip` [0..])
            return x0
    
        cemptyArray :: t -> Ccodegen Lvalue
        cemptyArray t = do
            ct <- ctPtr t
            x0 <- cint 0
            x1 <- cassign ct (Cint 0)
            cpair CtInt ct x0 x1
        
        carrayRead :: t -> Lvalue -> Lvalue -> Ccodegen Lvalue
        carrayRead t x i = cassign t (Cvar (Cindex (csnd x) i))
    
        ctake :: t -> Lvalue -> Lvalue -> Ccodegen Lvalue
        ctake t x1 x2 = do
            x3 <- cbinop "-" CtInt (cfst x1) x2
            z <- cint 0
            x4 <- cmax CtInt z x3
            ct <- ctPtr t
            cpair CtInt ct x4 (csnd x1)
    
        cdrop :: t -> Lvalue -> Lvalue -> Ccodegen Lvalue
        cdrop t x1 x2 = do
            x3 <- cbinop "-" CtInt (cfst x1) x2
            z <- cint 0
            x4 <- cmax CtInt z x3
            ct <- ctPtr t
            x5 <- cbinop "+" ct (csnd x1) x2
            cpair CtInt ct x4 x5
    
        cappend :: t -> Lvalue -> Lvalue -> Ccodegen Lvalue
        cappend t x1 x2 = do
            let l1 = clength x1
                l2 = clength x2
            l <- cbinop "+" CtInt l1 l2
            x3 <- cnewArray t l
            cmemcpy t (csnd x3) (csnd x1) l1
            ct <- ctPtr t
            x4 <- cbinop "+" ct l1 (csnd x3)
            cmemcpy t x4 (csnd x2) l2
            return x3
    
        cstr :: t -> t -> Lvalue -> Lvalue -> Ccodegen Lvalue
        cstr t1 t2 x1 x2 = do
            ct <- ctTuple [t1, t2]
            ct1 <- ctype t1
            cmap ct1 ct x1 (\ x1' -> cpair t1 t2 x1' x2)
    
        cmap :: t -> t -> Lvalue -> (Lvalue -> Ccodegen Lvalue) -> Ccodegen Lvalue
        cmap t1 t2 x f = do
            let x0 = clength x
            x1 <- cnewArray t2 x0
            ccForLoop x0 (\i -> do
                x2 <- carrayRead t1 x i
                x3 <- f x2
                carrayWrite x1 i (Cvar x3)  
                )
            return x1
    
        cfold :: t -> t -> Lvalue -> (Lvalue -> Lvalue -> Ccodegen Lvalue) -> Ccodegen Lvalue
        cfold t1 t2 x f = do
            let x0 = cfst x
                x1 = csnd x
                x2 = clength x1
            x3 <- cassign t1 (Cvar x0)
            ccForLoop x2 (\ i -> do
                x4 <- carrayRead t2 x1 i
                x5 <- f x3 x4
                cassign0 x3 (Cvar x5)
                )
            return x3
    
    instance Ctyped Ctype where
        ctype = return
    
    instance Ctyped Type where
        ctype t =
            case t of
                Tfloat -> return CtFloat
                Tpair t1 t2 -> ctTuple [t1, t2]
                Tunit ->  ctTuple ([] :: [Ctype])
                Teither t1 t2 -> ctVariant [t1, t2]
                Tempty -> ctVariant ([] :: [Ctype])
                Tvar _ -> ctTuple ([] :: [Ctype])
                TvarExistential _ -> undefined
                Tarray t0 -> ctArray t0
    
    cassign0 :: Lvalue -> Cexp -> Ccodegen Lvalue
    cassign0 x e = do
        cstmt (Cass0 x e)
        return x
    
    cfst :: Lvalue -> Lvalue
    cfst x = Caccess x 0
    
    csnd :: Lvalue -> Lvalue
    csnd x = Caccess x 1
    
    cmatchTuple :: Int -> Lvalue -> [Lvalue]
    cmatchTuple 0 _ = []
    cmatchTuple 1 x = [x]
    cmatchTuple n x = cfst x : cmatchTuple (n-1) (csnd x)
    
    cequals :: Lvalue -> Lvalue -> Ccodegen Lvalue
    cequals x1 x2 = do
        x3 <- cbinop "!=" CtInt x1 x2
        cassign (Teither Tunit Tunit) (CstructInitializer [x3])
    
    ccFun :: Ctype -> String -> Ctype -> String -> [Cstmt] -> Ccodegen String
    ccFun t1 f t2 x ss = do
        cdefinition (Cfunction t2 f t1 x ss)
        return f
    
    ccTypedef :: Ctypedef -> Ccodegen Ctype
    ccTypedef d = do
        let name = case d of
                CtdefUnion _ -> "union"
                CtdefStruct _ -> "struct"
                CtdefFun _ _ -> "fun"
                CtdefPtr _ -> "ptr"
        mt <- ccLookupTypedef d
        case mt of
            Nothing -> do
                i <- cfreshIdent name
                cdefinition (Ctypedef i d)
                return (CtDef i d)
            Just t -> return t
    
    cTypeLeft, cTypeRight :: Ctype -> Ctype
    cTypeLeft t =
        case t of
            CtDef _ (CtdefStruct [_, CtDef _ (CtdefUnion [t0, _])]) -> t0
            _ -> undefined
    cTypeRight t =
        case t of
            CtDef _ (CtdefStruct [_, CtDef _ (CtdefUnion [_, t1])]) -> t1
            _ -> undefined
    
    ccForLoop :: Lvalue -> (Lvalue -> Ccodegen a) -> Ccodegen ()
    ccForLoop n k = do
        i <- cint 0
        b <- cbinop "<" CtInt i n
        one <- cint 1
        ss <- cblock $ do
            k i
            cstmt (Cass0 i (CbinOp "+" i one))
            cstmt (Cass0 b (CbinOp "<" i n))
        cstmt (Cwhile b ss)
    
    ccFloat :: Float -> Ccodegen Lvalue
    ccFloat x = cassign Tfloat (Cfloat x)
    
    cint :: Int -> Ccodegen Lvalue
    cint x = cassign CtInt (Cint x)
    
    ccUnit :: Ccodegen Lvalue
    ccUnit = cassign Tunit (CstructInitializer [])
    
    -- (t1 + t2) * t3 -> t1 * t3 + t2 * t3
    ccDist :: Type -> Type -> Type -> Lvalue -> Lvalue -> Ccodegen Lvalue
    ccDist t1 t2 t3 x12 x3 =
        ceither (Teither (Tpair t1 t3) (Tpair t2 t3)) x12
            (\ x1 -> do
                x13 <- cpair t1 t3 x1 x3 
                cinl (Tpair t1 t3) (Tpair t2 t3) x13)
            (\ x2 -> do
                x23 <- cpair t2 t3 x2 x3
                cinr (Tpair t1 t3) (Tpair t2 t3) x23)
    
    ccMax :: Ctype -> Lvalue -> Lvalue -> Ccodegen Lvalue
    ccMax t x1 x2 = cassign t (CbinOp "max" x1 x2)
    
    ccEquals, ccLess :: Lvalue -> Lvalue -> Ccodegen Lvalue
    ccEquals x1 x2 = do 
        x <- cassign CtInt (CbinOp "!=" x1 x2)
        cassign (Teither Tunit Tunit) (CstructInitializer [x])
    ccLess x1 x2 = do 
        x <- cassign CtInt (CbinOp "<" x1 x2)
        cassign (Teither Tunit Tunit) (CstructInitializer [x])
    
    ciota :: Lvalue -> Ccodegen Lvalue
    ciota x = do
        cstmt (Ccomment "BEGIN iota")
        x0 <- cnewArray Tfloat x
        ccForLoop x (\ i -> carrayWrite x0 i (Cvar i))
        cstmt (Ccomment "END iota")
        return x0
    
    clength :: Lvalue -> Lvalue
    clength x = cfst x
    
    chead :: Type -> Lvalue -> Ccodegen Lvalue
    chead t x = do
        zero <- cint 0
        let l = clength x
        b <- ccEquals l zero
        ceither (Teither t Tunit) b (\ _ -> f zero) (\ _ -> g)
        where
            f z = do
                x0 <- carrayRead t x z
                cinl t Tunit x0
            g = do 
                x0 <- ccUnit
                cinr t Tunit x0
    
    carrayWrite :: Lvalue -> Lvalue -> Cexp -> Ccodegen Lvalue
    carrayWrite x i x0 = cassign0 (Cindex (csnd x) i) x0
    
    ccAxiom :: Lvalue -> Type -> ExpAxiom -> Type -> Ccodegen Lvalue
    ccAxiom x t1 e t2 =
        case e of
            ExpNum n -> ccFloat n
            ExpUnit -> ccUnit
            ExpEmpty -> return x
            ExpFst -> return $ cfst x
            ExpSnd -> return $ csnd x
            ExpInl -> cinl (inlType t2) (inrType t2) x
            ExpInr -> cinr (inlType t2) (inrType t2) x
            ExpDist -> 
                let tab = fstType t1
                    ta = inlType $ tab
                    tb = inrType $ tab
                    tc = sndType t1
                in ccDist ta tb tc (cfst x) (csnd x)
            ExpMinus -> cbinop "-" t2 (cfst x) (csnd x)
            ExpPlus -> cbinop "+" t2 (cfst x) (csnd x)
            ExpTimes -> cbinop "*" t2 (cfst x) (csnd x)
            ExpEquals -> cequals (cfst x) (csnd x)
            ExpVar -> return x
            ExpIota -> ciota x
            ExpMkList n -> cmkList n (eltType t2) x
            ExpLength -> return $ clength x
            ExpHead -> chead t2 x
            ExpTake -> ctake (eltType t2) (cfst x) (csnd x)
            ExpDrop -> cdrop (eltType t2) (cfst x) (csnd x)
            ExpEmptyArray -> cemptyArray (eltType t2)
            ExpAppend -> cappend (eltType t2) (cfst x) (csnd x)
            ExpStr -> cstr (eltType $ fstType t1) (sndType t1) (cfst x) (csnd x)
    
    compileExp :: Typed e => Lvalue -> Type -> Exp e -> Type -> Ccodegen Lvalue
    compileExp x t1 e t2 = do
        case e of
            ExpApp e1 e2 -> do
                x2 <- compile x e2
                compile x2 e1
            ExpPair e1 e2 -> do
                x1 <- compile x e1
                x2 <- compile x e2
                cpair (fstType t2) (sndType t2) x1 x2
            ExpEither e1 e2 ->
                ceither t2 x (\ x0 -> compile x0 e1) (\ x0 -> compile x0 e2)
            ExpAxiom e0 ->
                ccAxiom x t1 e0 t2
            ExpFun f -> do
                f' <- ccInstantiateTemplate t1 f t2
                ccall t2 (Cident f') x
            ExpMap e0 -> 
                cmap (inType e0) (outType e0) x (\ x0 -> compile x0 e0)
            ExpFold e0 -> 
                cfold t2 (fstType $ inType e0) x (\ x0 x1 -> do 
                    x2 <- cpair (fstType $ inType e0) t2 x0 x1
                    compile x2 e0)
    
    compileDef :: Typed e => Def e -> Ccodegen ()
    compileDef (Fundef t1' f t2' e) = do
        ccTemplate f $ \ t1 t2 -> do
            ct1 <- ctype t1
            ct2 <- ctype t2
            let x = "x"
                f' = cfunctionInstanceName ct1 f ct2
                mgu = [(t1', t1), (t2', t2)]
            cdefinition (CfunctionDecleration ct1 f' ct2)
            ss <- cblock $ do
                y <- compile (Cident x) (substituteMgu (solveConstraints f' mgu) e)
                cstmt (Creturn y)
            ccFun ct1 f' ct2 x ss
            return ()
    
    compilePrgm :: Typed e => Prgm e -> Ccodegen Lvalue
    compilePrgm (Prgm ds e) = do 
        x0 <- cdeclare Tunit 
        mapM compileDef ds >> compile x0 e
    
    satisfy :: (Char -> Bool) -> Parser Char
    satisfy p = Parser (\w -> 
        case w of
            c:w0 | p c -> [(c, w0)]
            _ -> [])
    
    many1 :: Parser e -> Parser [e]
    many1 p = (:) <$> p <*> many p
    
    sepBy1 :: Parser a -> Parser e -> Parser [e]
    sepBy1 op e = (:) <$> e <*> many (op *> e)
    
    sepBy :: Parser a -> Parser e -> Parser [e]
    sepBy op e = sepBy1 op e <|> pure []
    
    token :: String -> Parser String
    token t = parseWhitespace *> go t
        where
            go [] = pure ""
            go (c:cs) = (:) <$> satisfy (==c) <*> go cs
    
    parseWhitespace :: Parser String
    parseWhitespace = many (satisfy isSpace)
    
    parseInt :: Parser Int
    parseInt = read <$> many1 (satisfy isDigit)
    
    parseFloat :: Parser Float
    parseFloat = parse1 <|> parse2
        where
            readFloat x [] = readFloat x "0"
            readFloat x y = read (x ++ '.' : y)
            parse1 = readFloat <$> many1 (satisfy isDigit) <* satisfy (=='.') <*> many (satisfy isDigit)
            parse2 = fromIntegral <$> parseInt
    
    parseAxiom :: Parser ExpAxiom
    parseAxiom = 
        parseNum <|>
        parseUnit <|>
        parseEmpty <|>
        parseFst <|>
        parseSnd <|>
        parseInl <|>
        parseInr <|>
        parseDist <|>
        parseIota <|>
        parseLength <|>
        parseHead <|>
        parseTake <|>
        parseDrop <|>
        parseEmptyList <|>
        parseAppend <|>
        parseStr <|>
        -- parseMinus <|>
        -- parsePlus <|>
        -- parseTimes <|>
        -- parseEquals <|>
        parseVar
        where
            parseNum = ExpNum <$> (parseWhitespace *> parseFloat)
            parseUnit = const ExpUnit <$> token "unit"
            parseEmpty = const ExpEmpty <$> token "empty"
            parseFst = const ExpFst <$> token "fst"
            parseSnd = const ExpSnd <$> token "snd"
            parseInl = const ExpInl <$> token "inl"
            parseInr = const ExpInr <$> token "inr"
            parseDist = const ExpDist <$> token "dist"
            parseIota = const ExpIota <$> token "&"
            parseLength = const ExpLength <$> token "#"
            parseHead = const ExpHead <$> token "head"
            parseTake = const ExpTake <$> token "take"
            parseDrop = const ExpDrop <$> token "drop"
            parseEmptyList = const ExpEmptyArray <$> token "[]"
            parseAppend = const ExpAppend <$> token "append"
            parseStr = const ExpStr <$> token "str"
            -- parseMinus = const ExpMinus <$> token "-"
            -- parsePlus = const ExpPlus <$> token "+"
            -- parseTimes = const ExpTimes <$> token "*"
            -- parseEquals = const ExpEquals <$> token "=="
            parseVar = const ExpVar <$> token "x"
    
    {-
    x0 = fst
    x1 = fst snd
    x2 = fst snd snd
    
    x0. = x
    x1. = x snd
    x2. = x snd snd
    x3. = x snd snd snd
    -}
    parseProjection :: Parser Expr
    parseProjection = do
        token "x"
        i <- parseInt
        l <- ((satisfy (=='.') *> pure True) <|> pure False)
        return $ Expr (go i l)
        where
            go 0 False = ExpAxiom ExpFst
            go 0 True = ExpAxiom ExpVar
            go i b = ExpApp (Expr (go (i-1) b)) (Expr (ExpAxiom ExpSnd))
    
    parseAtom :: Parser Expr
    parseAtom = 
        parseProjection <|> 
        parseAxiom' <|> 
        parseParens <|> 
        parseFun <|> 
        parseMap <|> 
        parseFold
        where
            parseParens = token "(" *> parseExpr <* token ")"
            parseAxiom' = Expr . ExpAxiom <$> parseAxiom
            parseFun = Expr . ExpFun <$> parseIdent
            parseMap = Expr . ExpMap <$> (token "[" *> parseExpr <* token "]")
            parseFold = Expr . ExpFold <$> (token "<" *> parseExpr <* token ">")
    
    parseLeftAssoc :: Parser (Expr -> Expr -> Exp Expr) -> Parser Expr -> Parser Expr
    parseLeftAssoc op e = foldl f <$> e <*> many ((\ k e0 -> (`k` e0)) <$> op <*> e)
        where
            f e0 k = Expr (k e0)
    
    parseRightAssoc :: Parser (Expr -> Expr -> Exp Expr) -> Parser Expr -> Parser Expr
    parseRightAssoc op e = (f <$> e <*> ((\ k e0 -> (`k` e0)) <$> op <*> parseRightAssoc op e)) <|> e
        where
            f e0 k = Expr (k e0)
    
    
    expBinop :: ExpAxiom -> Expr -> Expr -> Exp Expr
    expBinop a e1 e2 = ExpApp (Expr (ExpAxiom a)) (Expr (ExpPair e1 e2))
    expTimes, expPlus, expMinus, expEquals :: Expr -> Expr -> Exp Expr
    expTimes = expBinop ExpTimes
    expPlus = expBinop ExpPlus
    expMinus = expBinop ExpMinus
    expEquals = expBinop ExpEquals
    
    parseApp :: Parser Expr
    parseApp = parseLeftAssoc (satisfy isSpace *> pure ExpApp) parseAtom
    
    parseTimes :: Parser Expr
    parseTimes = parseLeftAssoc (token "*" *> pure expTimes) parseApp
    
    parsePlus :: Parser Expr
    parsePlus = parseLeftAssoc (token "+" *> pure expPlus <|> token "-" *> pure expMinus) parseTimes
    
    parseEquals :: Parser Expr
    parseEquals = parseLeftAssoc (token "==" *> pure expEquals) parsePlus
    
    parsePair :: Parser Expr
    parsePair = parseRightAssoc (token "," *> pure ExpPair) parseEquals
    
    parseExpr :: Parser Expr
    parseExpr = parseRightAssoc (token "|" *> pure ExpEither) parsePair
    
    parseIdent :: Parser String
    parseIdent = parseWhitespace *> many1 (satisfy isAlpha)
    
    parseTypeAtom :: Parser Type
    parseTypeAtom = parseTfloat <|> parseTunit <|> parseTempty <|> parseTarray <|> parseTvar <|> parseParens
        where
            parseTfloat = const Tfloat <$> token "float"
            parseTunit = const Tunit <$> token "1"
            parseTempty = const Tempty <$> token "0"
            parseTarray = Tarray <$> (token "[" *> parseType <* token "]")
            parseTvar = parseWhitespace *> (Tvar . (\x -> x - ord 'a') . ord <$> satisfy isLower)
            parseParens = token "(" *> parseType <* token ")"
    
    parseTypeProduct :: Parser Type
    parseTypeProduct = (Tpair <$> parseTypeAtom <* token "*" <*> parseTypeProduct) <|> parseTypeAtom
    
    parseTypeSum :: Parser Type
    parseTypeSum = (Teither <$> parseTypeProduct <* token "+" <*> parseTypeSum) <|> parseTypeProduct
    
    parseType :: Parser Type
    parseType = parseTypeSum
    
    parseDef :: Parser (Def Expr)
    parseDef = do 
        f <- parseIdent
        _ <- token ":"
        t1 <- parseType
        _ <- token "|-"
        t2 <- parseType
        _ <- token "="
        e <- parseExpr
        _ <- token ";"
        return $ Fundef t1 f t2 e
    
    parsePrgm :: Parser (Prgm Expr)
    parsePrgm = Prgm <$> many parseDef <*> parseExpr
    
    emitPerformanceTimer :: [String]
    emitPerformanceTimer =
        [ "#include <windows.h>"
        , ""
        , "double PCFreq = 0.0;"
        , "__int64 CounterStart = 0;"
        , ""
        , "void StartCounter()"
        , "{"
        , "    LARGE_INTEGER li;"
        , "    if(!QueryPerformanceFrequency(&li))"
        , "      printf(\"QueryPerformanceFrequency failed!\\n\");"
        , ""
        , "    PCFreq = ((double) li.QuadPart)/1000.0;"
        , ""
        , "    QueryPerformanceCounter(&li);"
        , "    CounterStart = li.QuadPart;"
        , "}"
        , "double GetCounter()"
        , "{"
        , "    LARGE_INTEGER li;"
        , "    QueryPerformanceCounter(&li);"
        , "    return ((double) li.QuadPart-CounterStart)/PCFreq;"
        , "}"
        ]
    
    emit :: Type -> Ccodegen Lvalue -> String
    emit t c = 
        let (ds, ss, i, _) = runCcodegen c 0 []
        in
        unlines $
        [ "#include <stdio.h>"
        , "#include <stdlib.h>"
        , "#include <string.h>"
        , "#define MAX(a,b) (a <= b ? b : a)"
        , ""] ++
        emitPerformanceTimer ++
        concatMap emitDefinition ds ++
        [ "int main() {"
        , "  StartCounter();"
        ]
        ++ map ("  " ++) (emitBlock ss)
        ++ ["  double time = GetCounter();"]
        ++ map ("  " ++) (emitPrintResult 0 t i)
        ++
        [ "  printf(\"\\nelapse: %f ms\\n\", time);"
        , "}"
        ]
    
    emitTypedef :: String -> Ctypedef -> [String]
    emitTypedef i d =
        case d of
            CtdefStruct ts -> ["typedef struct {" ++ concatMap ((++"; ") . (\ (t, x) -> emitDecleration t (("x" ++ show x)))) (zip ts [0 :: Int ..]) ++ "} " ++ i ++ ";"]
            CtdefUnion ts -> ["typedef union {" ++ concatMap ((++"; ") . (\ (t, x) -> emitDecleration t (("x" ++ show x)))) (zip ts [0 :: Int ..]) ++ "} " ++ i ++ ";"]
            CtdefFun t1 t2 -> ["typedef " ++ emitType 0 t1 ++ "(*" ++ i ++ ")(" ++ emitType 0 t2 ++ ");"]
            CtdefPtr t1 -> ["typedef " ++ emitType 1 t1 ++ i ++ ";"]
    
    emitDefinition :: Cdefinition -> [String]
    emitDefinition (Cfunction t f tx x ss) =
        [emitDecleration t f ++ "(" ++ emitDecleration tx x ++ ") {"]
        ++ map ("  " ++) (emitBlock ss)
        ++ 
        ["}"]
    emitDefinition (Ctypedef i d) = emitTypedef i d
    emitDefinition (Ctemplate _ _) = []
    emitDefinition (CfunctionDecleration t1 f t2) = [emitDecleration t2 f ++ "(" ++ emitType 0 t1 ++ ");"]
    
    emitPrintResult :: Int -> Type -> Lvalue -> [String]
    emitPrintResult loopDepth t i =
            case t of
                Tfloat -> ["printf(\"%f\", " ++ emitLvalue i ++ ");"]
                Tpair t1 t2 -> 
                    ["printf(\"(\");"] ++ 
                    (intercalate ["printf(\", \");"] (map (\ (ti, ii :: Int) -> emitPrintResult loopDepth ti (Cident $ emitLvalue i ++ ".x" ++ show ii)) (zip [t1, t2] [0..]))) ++
                    ["printf(\")\");"]
                Tunit -> ["printf(\"()\");"]
                Teither t1 t2 ->
                    [ "printf(\"#%d \", " ++ emitLvalue i ++ ".x0);"
                    , "switch(" ++ emitLvalue i ++ ".x0) {"] ++
                    concatMap ( \(ti, ii :: Int) -> ["  case " ++ show ii ++ ": {"] ++ (map ("    "++) $ emitPrintResult loopDepth ti (Cident $ emitLvalue i ++ ".x1.x" ++ show ii) ++ ["    break;", "  }"])) (zip [t1, t2] [0..]) ++
                    ["}"]
                Tempty -> ["printf(\"_|_\");"]
                Tvar _ -> ["printf(\"()\");"]
                TvarExistential _ -> undefined
                Tarray t0 -> 
                    let j = "i" ++ show loopDepth
                    in
                    ["printf(\"[\");"
                    ,"for(int " ++ j ++ " = 0; " ++ j ++ " < " ++ emitLvalue i ++ ".x0; ++" ++ j ++ ") {"
                    ,"  if(" ++ j ++ " > 0) printf(\",\");"
                    ] ++
                    map ("  "++) (emitPrintResult (loopDepth + 1) t0 (Cident $ emitLvalue i ++ ".x1[" ++ j ++ "]")) ++
                    ["}"
                    ,"printf(\"]\");"]
                -- Tvar _ -> ["printf(\"%d\", " ++ unCident i ++ ");"]
                -- TvarExistential _ -> ["printf(\"%d\", " ++ unCident i ++ ");"]
    
    emitBlock :: [Cstmt] -> [String]
    emitBlock = concatMap emitStmt
    
    emitStmt :: Cstmt -> [String]
    emitStmt s =
        case s of
            Cdecl t i -> [emitDecleration t i ++ ";"]
            Cass t i e -> [emitDecleration t i ++ " = " ++ emitExp e ++ ";"]
            Cass0 i e -> [emitLvalue i ++ " = " ++ emitExp e ++ ";"]
            Creturn i -> ["return " ++ emitLvalue i ++ ";"]
            Cswitch i ss -> 
                ["switch(" ++ emitLvalue i ++ ") {"] ++
                concatMap (\ (s0, ii :: Int) -> ["  case " ++ show ii ++ ": {"] ++ (map ("    "++) $ emitBlock s0) ++ ["    break;", "  }"]) (zip ss [0..]) ++
                ["}"]
            Cwhile i ss ->
                ["while(" ++ emitLvalue i ++ ") {"] ++
                map ("  "++) (emitBlock ss) ++
                ["}"]
            Csyscall f is ->
                [f ++ "(" ++ intercalate ", " (map emitLvalue is) ++ ");"]
            Ccomment txt -> ["// " ++ txt]
    
    emitType :: Int -> Ctype -> String
    emitType indi t =
        let x = replicate indi '*'
        in
        case t of
            CtFloat -> "float " ++ x
            CtInt -> "int " ++ x
            CtDef i _ -> i ++ " " ++ x
    
    emitDecleration :: Ctype -> String -> String
    emitDecleration t i = emitType 0 t ++ i
    
    emitExp :: Cexp -> String
    emitExp e =
        case e of
            Cvar x -> emitLvalue x
            Cfloat x -> show x ++ "f"
            Cint x -> show x
            CbinOp op x1 x2 -> "(" ++ emitLvalue x1 ++ " " ++ op ++ " " ++ emitLvalue x2 ++ ")"
            Cmax x1 x2 -> "MAX(" ++ emitLvalue x1 ++ ", " ++ emitLvalue x2 ++ ")"
            Ccall f x -> emitLvalue f ++ "(" ++ emitLvalue x ++ ")"
            CstructInitializer es -> "{" ++ intercalate ", " (map (emitExp . Cvar) es) ++ "}"
            CunionInitializer tag x -> "{" ++ show tag ++ ", " ++ emitLvalue x ++ "}"
            Cmalloc x -> "malloc(" ++ emitLvalue x ++ ")"
            Cdereference x -> "*" ++ emitLvalue x
            CsizeOf ct -> "sizeof(" ++ emitType 0 ct ++ ")"
    
    infer :: Expression e => Prgm e -> Prgm TypedExp
    infer p =
        let (a, b, _) = runTypeInf (typeInferencePrgm p) 0
        in substituteMgu (solveConstraints (intercalate "\n" $ showPrgm p) b) a
    
    forgetTypes :: TypedExp -> Expr
    forgetTypes (TypedExp _ e _) = Expr (forgetTypes <$> e)
    
    class Expression e where
        interpret :: MonadFail m => ValueContext m -> Value -> e -> m Value
        prettyPrint :: e -> String
        typeInference :: TypeContext -> Type -> e -> TypeInf TypedExp
    
    instance Expression e => Expression (Exp e) where
        interpret = interpretExp
        prettyPrint = prettyPrintExp
        typeInference = typeInferenceExp
    
    instance Expression Expr where
        interpret g v = interpret g v . unExpr
        prettyPrint = prettyPrint . unExpr
        typeInference g t e = typeInference g t (unExpr e)
    
    instance Expression TypedExp where
        interpret g v (TypedExp _ e _) = interpret g v e
        prettyPrint (TypedExp t1 e t2) = show t1 ++ " |- " ++ prettyPrint (forgetTypes <$> e) ++ " : " ++ show t2
        typeInference g t (TypedExp _ e _) = typeInference g t e
    
    class (TypeSubst e, Expression e) => Typed e where
        inType :: e -> Type
        outType :: e -> Type
        compile :: Lvalue -> e -> Ccodegen Lvalue
    
    instance Typed TypedExp where
        inType (TypedExp t _ _) = t
        outType (TypedExp _ _ t) = t
        compile x (TypedExp t1 e t2) = compileExp x t1 e t2
    
    parse :: String -> Prgm Expr
    parse w =
        case runParser (parsePrgm <* parseWhitespace) w of
            [(e, "")] -> e
            r -> error (show r)
    
    {-
    i ::= n
    float ::= int | int . | int . int 
    
    atom ::= float | atom atom | 
        [sum, ..., sum] | atom.i | 
        [#i atom] | atom?[sum, ..., sum] | 
        x | {sum}
    product ::= atom * product
    sum ::= product + sum
    exp ::= sum 
    
    [1,2,3]  -- product intro
    x.1      -- product elim
    
    [#2 x]   -- sum intro
    x?[1,2]  -- sum elim 
    
    bind : (a + b) * (b -> a + c) -> a + b * c
    bind {
        x0?[
            #0 x',
            (x1 x')?[
                #0 x'',
                #1 [x', x'']
            ]
        ]
    }
    
    fromMaybe : (a + b) * b -> b
    {
        x0?[x1, x']
    }
    
    pure : a -> e + a
    {
        [#1 x]
    }
    
    fmap : (a -> b) * (e + a) -> e + b
    {
        x1?[#0 x', #1 (x0 x')]
    }
    
    join : e + (e + a) -> e + a
    {
        x?[#0 x', x']
    }
    
    
    swap : a + b -> b + a
    swap {
        x?[#1 x', #0 x']
    }
    
    idemp : a + a -> a
    idemp {
        x?[x', x']
    }
    
    type bool = 1 + 1
    type maybe a = 1 + a
    type list a = 1 + a * [list a]
    
    -----------
    t |- box : [t]
    
    ---------------
    [t] |- unbox : t
    
    -------------
    t |- unit : 1
    
    --------------
    0 |- empty : t
    
    
    t |- of : list t
    of = inr (x, box (inr unit))
    
    list a * list a |- append : list a
    append = (snd + fst fst * append (unbox (snd fst) * snd)) dist -- 1 * list a + (a * [list a]) * list a
    
    bool * bool |- ||, &&, => : bool
    || = (inl fst + snd) dist
    && = (snd + inr fst) dist
    => = (snd + inl fst) dist
    => = || (not fst) snd
    
    bool |- not : bool
    not = inr + inl
    
    e ::= e , e | unit | e `|` e | empty | fst | snd | inl | inr | dist | x | e e | float | - | + | * | == | ident
    t ::= 1 | 0 | t `|` t | t , t | float
    
    fac = (1.0 + fac (snd x - 1.0)) (dist (x == 0.0 * x))
    
    
    (a + 1) * a |- fromMaybe : a
    fromMaybe = (fst | snd) distR
    
    a * (a + 1) |- fromMaybe : a
    fromMaybe = (snd | fst) distL
    
    ------------------    -------------------
    a * a |- fst : a    1 * a |- snd x : a
    ------------------------------------------                          ------------------------------------------
    a * a + 1 * a |- fst + snd : a                                       (a + 1) * a |- dist : a * a + 1 * a
    ---------------------------------------------------------------------------------------------------------------
                                     (a + 1) * a |- (fst + snd) dist : a
    
    type tree a = 1 + a + {tree a} * {tree a}
    
    (a + b) + b |-
    join : a + b
    join = (inl | inr) | inr
    
    float + b |- double : float + b
    double = + (x, x) | inr
    
    int |- fib : int
    fib =
        if x == 0 || x == 1  
        then 1
        else fib (x - 1) +f fib (x - 2)
    
    fac = (1 + fac (x - 1)) (x == 0)
    
    [1000 * ({1 + 2} (x = 1))]
    
     T1                T2
    -----------------------------------------
    float + b |- +f (x * x) + inr : float + b
    
    
    t1 |- e1 : t2   t |- e2 : t1
    ----------------------------
    t |- e1 e2 : t2
    
    T1
    ---------------------------            T3
    float * float |- +f : float        float |- x * x : float * float
    -----------------------------------------------------------------
    float |- +f (x * x) : float
    
    
    T2
    --------------------
    b |- inr : float + b
    
    
    T3
    ------------------   ------------------
    float |- x : float   float |- x : float
    ---------------------------------------
    float |- x * x : float * float
    
    a * b * c |- .1 : b
    .1 = snd fst
    
    .0 = fst
    .1 = fst snd           snd
    .2 = fst snd snd       snd snd
    .3 = fst snd snd snd   snd snd snd
    
    #0 = inl
    #1 = inr inl
    
    ----------------         -------------------------
    b * c |- fst : b          a * b * c |- snd : b * c
    --------------------------------------------------
    a * b * c |- fst snd : b
    
    
    t1 |- e1 : t2   t |- e2 : t1
    ----------------------------
       t |- e1 e2 : t2
    
    
                                       ---------------------------------------
    b * (c * d) |- fst snd : c         a * (b * (c * d)) |- snd : b * (c * d)
    --------------------------------------------------------------------------
    a * b * c * d |- (fst snd) snd : c
    
    fun<<iota>> = iota
    .
    .
    .
    fun<<t1 |- e : t2>> = <<t2>> f(<<t>> x) {return <<e>> x;}
    
    <<e1 e2>> x = <<e1>> (<<e2>> x)
    <<&>> x = iota(x)
    <<x>> x = x
    <<e1, e2>> (x : t) = {<<e1>> x, <<e2>> x}  -- x appears twice
    <<e1 | e2>> (x : t) = (x.x0 == 0 ? <<e1>> x.x1 : <<e2>> x.x1) -- x appears twice
    <<inl e>> x = {0, <<e>> x}
    <<inr e>> x = {1, <<e>> x}
    <<fst e>> x = x.x0
    <<snd e>> x = x.x1
    <<unit>> (x : t) = {}  -- x is lost
    <<empty>> x = exit(1) -- x is lost
    <<map e>> x = map(fun<<e>>, x)
    <<fold e>> x = fold(fun<<e>>, x) -- forces x
    <<x>> x = (x.x0 + x.x1) -- x appears twice
    
    
    
    
    -}
    
    newtype Gen w a = Gen { runGen :: Int -> ((w, Sum Int), a)} deriving Functor
    
    instance Monoid w => Applicative (Gen w) where
        pure x = Gen (pure (pure x))
        f <*> x = Gen $ \i -> runGen f i <*> runGen x (getSum $ snd $ fst $ runGen f i)
    
    instance Monoid w=> Monad (Gen w) where
        return = pure
        x >>= f = Gen $ \i -> runGen x i >>= (\ a -> runGen (f a) (getSum $ snd $ fst $ runGen x i))
    
    fresh :: Monoid w => Gen w Int
    fresh = Gen $ \i -> ((mempty, 1), i)
    
    emitCode :: Monoid w => w -> Gen w ()
    emitCode w = Gen $ \i -> ((w, 0), ())
    
    type T = Gen [String]
    
    transpile :: Typed e => e -> [String] -> T [String]
    transpile = undefined
    
    unionTypeSize :: Type -> Type -> Int
    unionTypeSize t1 t2 = 1 + typeSize t1 + typeSize t2
    
    unionDecl :: Int -> Type -> Type -> T [String]
    unionDecl ptr t1 t2 = do
        x <- (("tag"++) . show) <$> fresh
        emitCode ["int " ++ replicate ptr '*' ++ x ++ ";"]
        xs1 <- decl ptr t1
        xs2 <- decl ptr t2
        return (x : xs1 ++ xs2)
    
    inlXs :: Type -> Type -> [String] -> [String]
    inlXs t1 _ xs = take (typeSize t1) (tail xs)
    
    inrXs :: Type -> Type -> [String] -> [String]
    inrXs t1 _ xs = drop (typeSize t1) (tail xs)
    
    typeSize :: Type -> Int
    typeSize t =
        case t of
            Tfloat -> 1
            Tpair t1 t2 -> typeSize t1 + typeSize t2
            Tunit -> 0
            Teither t1 t2 -> unionTypeSize t1 t2
            Tempty -> 0
            Tvar _ -> undefined
            TvarExistential _ -> undefined
            Tarray t0 -> 1 + typeSize t0
    
    decl :: Int -> Type -> T [String]
    decl ptr t =
        let p = replicate ptr '*'
        in
        case t of
            Tfloat -> do
                x <- (("x"++) . show) <$> fresh
                emitCode ["float " ++ p ++ x ++ ";"]
                return [x]
            Tpair t1 t2 -> do
                xs1 <- decl ptr t1
                xs2 <- decl ptr t2
                return (xs1 ++ xs2)
            Tunit -> return []
            Teither t1 t2 -> unionDecl ptr t1 t2
            Tempty -> return []
            Tvar _ -> undefined
            TvarExistential _ -> undefined
            Tarray t0 -> do
                x <- (("sz"++) . show) <$> fresh
                xs <- decl (ptr + 1) t0
                emitCode ["int " ++ p ++ x ++ ";"]
                return (x : xs)
    
    assign :: [String] -> [String] -> T ()
    assign ys xs = emitCode (zipWith (\ y x -> y ++ " = " ++ x ++ ";") ys xs)
    
    {-
    float x1 x2 x3
    int x1 x2 x3
    
    
    
    -}
    transpileExp :: TypedExp -> [String] -> T [String]
    transpileExp (TypedExp t1 e t2) xs =
        case e of
            ExpApp e1 e2 -> do
                xs0 <- transpile e2 xs
                transpile e1 xs0
            ExpPair e1 e2 -> do
                xs0 <- transpile e1 xs
                xs1 <- transpile e2 xs
                return (xs0 ++ xs1)
            ExpEither e1 e2
                | Teither t10 t11 <- t1 -> do
                    ys <- decl 0 t2
                    lbl2 <- (("l"++) . show) <$> fresh
                    lbl1 <- (("l"++) . show) <$> fresh
                    emitCode ["if(" ++ head xs ++ ") goto " ++ lbl2 ++ ";"]
                    xs1 <- transpile e1 (inlXs t10 t11 xs)
                    assign ys xs1
                    emitCode 
                        [ "goto " ++ lbl1 ++ ";"
                        , lbl2 ++ ":"
                        ]
                    xs2 <- transpile e2 (inrXs t10 t11 xs)
                    assign ys xs2
                    emitCode 
                        [ lbl1 ++ ":"
                        ]
                    return ys
                    {-
                    if (x0) goto lbl2
                    <<e1>>
                    goto lbl1
                    lbl2 :
                    <<e2>>
                    lbl1 :
                    -}
                | otherwise -> undefined
            ExpAxiom e0 ->
                case e0 of
                    ExpNum x -> return [show x]
                    ExpUnit -> return []
                    ExpEmpty -> return []
                    ExpFst
                        | Tpair t10 _ <- t1 -> return $ take (typeSize t10) xs
                        | otherwise -> undefined
                    ExpSnd
                        | Tpair t10 _ <- t1 -> return $ drop (typeSize t10) xs
                        | otherwise -> undefined
                    ExpInl 
                        | Teither _ t11 <- t2 -> do
                            xs11 <- decl 0 t11
                            return ("0" : xs ++ xs11)
                        | otherwise -> undefined
                    ExpInr
                        | Teither t10 _ <- t2 -> do
                            xs10 <- decl 0 t10
                            return ("1" : xs10 ++ xs)
                        | otherwise -> undefined
                    ExpDist
                        | Tpair (Teither t3 t4) t5 <- t1 -> do
                            let sz3 = typeSize t3
                                sz4 = typeSize t4
                                sz5 = typeSize t5
                            return (take (1 + sz3) xs ++ drop (1 + sz3 + sz4) xs ++ drop (1 + sz3) xs)
                        | otherwise -> undefined 
                    ExpMinus -> return [xs !! 0 ++ " - " ++ xs !! 1]
                    ExpPlus -> return [xs !! 0 ++ " + " ++ xs !! 1]
                    ExpTimes -> return [xs !! 0 ++ " * " ++ xs !! 1]
                    ExpEquals -> return [xs !! 0 ++ " != " ++ xs !! 1]
                    ExpVar -> return xs
                    ExpIota -> _
                    ExpMkList n -> _
                    ExpLength -> _
                    ExpHead -> _
                    ExpTake -> _
                    ExpDrop -> _
                    ExpEmptyArray -> _
                    ExpAppend -> _
                    ExpStr -> _
            ExpFun f -> _
            ExpMap e0 -> _
            ExpFold e0 -> _
        