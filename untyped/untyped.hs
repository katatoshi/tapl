import Data.Set

type VarName = String

data Term
    = TmVar VarName
    | TmAbs VarName Term
    | TmApp Term Term
    deriving (Eq, Show)

fv :: Term -> Set VarName
fv (TmVar x) = singleton x
fv (TmAbs x t1) = fv t1 \\ singleton x
fv (TmApp t1 t2) = fv t1 `union` fv t2

termSubst :: VarName -> Term -> Term -> Term
termSubst x s (TmVar y)
    | x == y = s
    | x /= y = TmVar y
termSubst x s (TmApp t1 t2) = TmApp (termSubst x s t1) (termSubst x s t2)
termSubst x s (TmAbs y t1)
    | x == y = TmAbs y t1
    | x /= y && (not (y `elem` fv s) || not (x `elem` fv t1)) = TmAbs y $ termSubst x s t1
    | otherwise = let z = x ++ y in TmAbs z $ termSubst x s $ termSubst y (TmVar z) t1

isval :: Term -> Bool
isval (TmAbs _ _) = True
isval _ = False

eval1 :: Term -> Maybe Term
eval1 (TmApp (TmAbs x t12) v2) | isval v2 = Just $ termSubst x v2 t12
eval1 (TmApp v1 t2) | isval v1 = do
    t2' <- eval1 t2
    return $ TmApp v1 t2'
eval1 (TmApp t1 t2) = do
    t1' <- eval1 t1
    return $ TmApp t1' t2
eval1 _ = Nothing

eval :: Term -> Either String Term
eval t = case eval1 t of Just s            -> eval s
                         Nothing | isval t -> Right t
                         _                 -> Left $ show t ++ " is a non-value normal form."

