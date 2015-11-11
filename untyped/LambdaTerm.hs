module LambdaTerm
( LambdaTerm(..)
, isval
, eval1
, eval
) where

import Data.Set

type VarName = String

data LambdaTerm
    = TmVar VarName
    | TmAbs VarName LambdaTerm
    | TmApp LambdaTerm LambdaTerm
    deriving (Eq)

instance Show LambdaTerm where
    show (TmVar x) = x
    show (TmAbs x t1) = "(\\" ++ (show (TmVar x)) ++ "." ++ (show t1) ++ ")"
    show (TmApp t1 t2) = "(" ++ (show t1) ++ " " ++ (show t2) ++ ")"

-- 与えられたラムダ項の自由変数の集合を求める
fv :: LambdaTerm -> Set VarName
fv (TmVar x) = singleton x
fv (TmAbs x t1) = fv t1 \\ singleton x
fv (TmApp t1 t2) = fv t1 `union` fv t2

-- 小野テキストの代入における z を求める（変数名に "'" を加えていくだけ）
alterVarName :: LambdaTerm -> LambdaTerm -> VarName -> VarName
alterVarName t1 s y
    | y' `elem` (fv t1 `union` fv s) = alterVarName t1 s y'
    | otherwise = y'
    where y' = y ++ "'"

-- 代入（小野テキスト版）
termSubst :: VarName -> LambdaTerm -> LambdaTerm -> LambdaTerm
termSubst x s (TmVar y)
    | x == y = s
    | x /= y = TmVar y
termSubst x s (TmApp t1 t2) = TmApp (termSubst x s t1) (termSubst x s t2)
termSubst x s (TmAbs y t1)
    | x == y = TmAbs y t1
    | x /= y && (not (y `elem` fv s) || not (x `elem` fv t1)) = TmAbs y $ termSubst x s t1
    | otherwise = let z = alterVarName t1 s y in TmAbs z $ termSubst x s $ termSubst y (TmVar z) t1

-- 値かどうか判定する
isval :: LambdaTerm -> Bool
isval (TmAbs _ _) = True
isval _ = False

-- 1 ステップ評価関数 - Monad（do 記法）版
eval1 :: LambdaTerm -> Maybe LambdaTerm
eval1 (TmApp (TmAbs x t12) v2) | isval v2 = Just $ termSubst x v2 t12
eval1 (TmApp v1 t2) | isval v1 = do
    t2' <- eval1 t2
    return $ TmApp v1 t2'
eval1 (TmApp t1 t2) = do
    t1' <- eval1 t1
    return $ TmApp t1' t2
eval1 _ = Nothing

-- 1 ステップ評価関数 - Applicative スタイル版（Monad 版と関数としては等しい）
eval1' :: LambdaTerm -> Maybe LambdaTerm
eval1' (TmApp (TmAbs x t12) v2) | isval v2 = Just $ termSubst x v2 t12
eval1' (TmApp v1 t2) | isval v1 = let t2' = eval1' t2 in TmApp <$> pure v1 <*> t2'
eval1' (TmApp t1 t2) = let t1' = eval1' t1 in TmApp <$> t1' <*> pure t2
eval1' _ = Nothing

-- 評価関数
eval :: LambdaTerm -> Either LambdaTerm LambdaTerm
eval t = case eval1 t of Just s -> eval s
                         Nothing | isval t -> Right t
                         _ -> Left t

