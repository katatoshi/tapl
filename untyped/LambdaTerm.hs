{-# LANGUAGE FlexibleContexts #-}

module LambdaTerm
( LambdaTerm
, isval
, eval1
, eval
, evalWithLog
) where

import Control.Monad.Writer
import Data.Set

-- http://unbui.lt/#!/post/haskell-parsec-basics
import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)

parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule text = Parsec.parse rule "" text

-- BNF はこんな感じ？
-- ========================================
-- varName ::= 'a' | ... | 'z'
--           | varName '\''
--
-- term ::= varName
--        | '(' '\' varName '.' term ')'
--        | '(' term ' ' term ')'
-- ========================================

varNameParse :: Parsec.Parsec String () String
varNameParse = do
    lowerLetter <- Parsec.lower
    primes <- Parsec.many (Parsec.char '\'')
    return (lowerLetter : primes)

tmVarParse :: Parsec.Parsec String () LambdaTerm
tmVarParse = do
    varName <- varNameParse
    return $ TmVar varName

tmAbsParse :: Parsec.Parsec String () LambdaTerm
tmAbsParse = do
    Parsec.char '('
    Parsec.char '\\'
    varName <- varNameParse
    Parsec.char '.'
    term <- lambdaTermParse
    Parsec.char ')'
    return $ TmAbs varName term

tmAppParse :: Parsec.Parsec String () LambdaTerm
tmAppParse = do
    Parsec.char '('
    term1 <- lambdaTermParse
    Parsec.space
    term2 <- lambdaTermParse
    Parsec.char ')'
    return $ term1 `TmApp` term2

lambdaTermParse :: Parsec.Parsec String () LambdaTerm
lambdaTermParse = Parsec.try tmVarParse <|> Parsec.try tmAbsParse <|> Parsec.try tmAppParse

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

fv :: LambdaTerm -> Set VarName
fv (TmVar x) = singleton x
fv (TmAbs x t1) = fv t1 \\ singleton x
fv (TmApp t1 t2) = fv t1 `union` fv t2

alterVarName :: LambdaTerm -> LambdaTerm -> VarName -> VarName
alterVarName t1 s y
    | y' `elem` (fv t1 `union` fv s) = alterVarName t1 s y'
    | otherwise = y'
    where y' = y ++ "'"

termSubst :: VarName -> LambdaTerm -> LambdaTerm -> LambdaTerm
termSubst x s (TmVar y)
    | x == y = s
    | x /= y = TmVar y
termSubst x s (TmApp t1 t2) = TmApp (termSubst x s t1) (termSubst x s t2)
termSubst x s (TmAbs y t1)
    | x == y = TmAbs y t1
    | x /= y && (not (y `elem` fv s) || not (x `elem` fv t1)) = TmAbs y $ termSubst x s t1
    | otherwise = let z = alterVarName t1 s y in TmAbs z $ termSubst x s $ termSubst y (TmVar z) t1

isval :: LambdaTerm -> Bool
isval (TmAbs _ _) = True
isval _ = False

eval1 :: LambdaTerm -> Maybe LambdaTerm
eval1 (TmApp (TmAbs x t12) v2) | isval v2 = Just $ termSubst x v2 t12
eval1 (TmApp v1 t2) | isval v1 = do
    t2' <- eval1 t2
    return $ TmApp v1 t2'
eval1 (TmApp t1 t2) = do
    t1' <- eval1 t1
    return $ TmApp t1' t2
eval1 _ = Nothing

-- one step evaluation - Applicative version
eval1' :: Term -> Maybe Term
eval1' (TmApp (TmAbs x t12) v2) | isval v2 = Just $ termSubst x v2 t12
eval1' (TmApp v1 t2) | isval v1 = let t2' = eval1' t2 in TmApp <$> Just v1 <*> t2'
eval1' (TmApp t1 t2) = let t1' = eval1' t1 in TmApp <$> t1' <*> Just t2
eval1' _ = Nothing

eval :: LambdaTerm -> Either LambdaTerm LambdaTerm
eval t = case eval1 t of Just s -> eval s
                         Nothing | isval t -> Right t
                         _ -> Left t

evalWithLog :: LambdaTerm -> Writer [String] (Either LambdaTerm LambdaTerm)
evalWithLog t = case eval1 t of
    Just s -> do
        tell [show t]
        evalWithLog s
    Nothing | isval t -> do
        tell [show t]
        return $ Right t
    _ -> do
        tell [show t]
        tell ["The reduced term is a non-value normal form."]
        return $ Left t

printEvalStep :: LambdaTerm -> IO ()
printEvalStep t = mapM_ putStrLn $ snd $ runWriter $ evalWithLog t

printEvalStepByGetLine :: IO ()
printEvalStepByGetLine = do
    str <- getLine
    Right term <- return $ parse lambdaTermParse str
    printEvalStep term

-- パースに失敗したら場合の処理はしていない
--
-- このファイルをロードした上で、tapl5-3.pdf の評価の例を ghci 上で次のように確認できる
--
-- ghci> let t = parseLambdaTerm "((\\x.x) (\\z.((\\x.x) z)))"
-- ghci> printEvalStep t
-- ((\x.x) (\z.((\x.x) z)))
-- (\z.((\x.x) z))
--
-- 値コンストラクタを組み合わせて作るよりは書きやすく読みやすいと思う
--
parseLambdaTerm :: String -> LambdaTerm
parseLambdaTerm str = case parse lambdaTermParse str of
    Right term -> term

