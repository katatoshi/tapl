{-# LANGUAGE FlexibleContexts #-}

module Lambda
( LambdaTerm
, isval
, eval1
, eval
, evalWithLog
) where

-- http://unbui.lt/#!/post/haskell-parsec-basics
import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)

import Control.Monad.Writer

import LambdaTerm

-- rule に照らして text をパースする関数。結果の型 a は任意（ここで FlexibleContexts が必要？）
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

