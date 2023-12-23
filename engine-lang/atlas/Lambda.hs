module Lambda where

data BaseType = IntType

data Type 
    = FunType Type Type
    | BaseType BaseType

data Exp
    = AnnoExp Exp Type
    | Variable String
    | App Exp Exp
    | Lambda String Exp

data Value
    = Neutral Neutral
    | LambdaVal String Value

data Neutral
    = NeutralVariable String
    | NeutralApp Neutral Value

eval :: Exp -> Maybe Value
eval e =
    case e of
        AnnoExp e _ -> eval e
        Variable _ -> Nothing
        App e1 e2 -> 
            case eval e1 of
                Just (LambdaVal x v1) -> eval (_ x e2)
                Just (Neutral n) -> do
                    v2 <- eval e2
                    return $ Neutral (NeutralApp n v2)
                _ -> Nothing
        Lambda x e0 -> do
            v0 <- eval e0
            return $ LambdaVal x v0
        Variable x -> Neutral (NeutralVariable x)
        