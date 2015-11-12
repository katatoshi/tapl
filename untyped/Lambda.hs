{-# LANGUAGE FlexibleContexts #-}

module Lambda
( LambdaTerm
, isval
, eval1
, eval
, evalWithLog
, printEvalStep
, readLambdaTerm 
, parseLambdaTerm 
) where

--
-- このファイルをロードした上で下記のラムダ項の評価ステップを表示すれば、
-- TaPL p.43 の値呼び戦略の評価ステップを確認することができる
--
-- ghci> :l Lambda
-- [1 of 2] Compiling LambdaTerm       ( LambdaTerm.hs, interpreted )
-- [2 of 2] Compiling Lambda           ( Lambda.hs, interpreted )
-- Ok, modules loaded: Lambda, LambdaTerm.
-- ghci> let t = parseLambdaTerm "((\\x.x) (\\z.((\\x.x) z)))"
-- ghci> t
-- ((\x.x) (\z.((\x.x) z)))
-- ghci> printEvalStep t
-- ((\x.x) (\z.((\x.x) z)))
-- (\z.((\x.x) z))
--

import LambdaTerm

import Control.Monad.Writer

import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)

-- 評価関数 - 評価結果と共に各ステップでの評価結果のログを返す
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

-- 受け取ったラムダ項の評価ステップを標準出力に出力する関数
printEvalStep :: LambdaTerm -> IO ()
printEvalStep t = mapM_ putStrLn $ snd $ runWriter $ evalWithLog t

-- 以下、ラムダ項のパーサ実装

-- parse 関数 - http://unbui.lt/#!/post/haskell-parsec-basics
parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule text = Parsec.parse rule "" text

-- BNF
--
-- varName ::= 'a' | ... | 'z'
--           | varName '\''
--
-- term ::= varName
--        | '(' '\\' varName '.' term ')'
--        | '(' term ' ' term ')'
--

-- 変数名パーサ
varNameParse :: Parsec.Parsec String () String
varNameParse = do
    lowerLetter <- Parsec.lower
    primes <- Parsec.many (Parsec.char '\'')
    return (lowerLetter : primes)

-- 変数パーサ
tmVarParse :: Parsec.Parsec String () LambdaTerm
tmVarParse = do
    varName <- varNameParse
    return $ TmVar varName

-- ラムダ抽象パーサ
tmAbsParse :: Parsec.Parsec String () LambdaTerm
tmAbsParse = do
    Parsec.char '('
    Parsec.char '\\'
    varName <- varNameParse
    Parsec.char '.'
    term <- lambdaTermParse
    Parsec.char ')'
    return $ TmAbs varName term

-- 関数適用パーサ
tmAppParse :: Parsec.Parsec String () LambdaTerm
tmAppParse = do
    Parsec.char '('
    term1 <- lambdaTermParse
    Parsec.space
    term2 <- lambdaTermParse
    Parsec.char ')'
    return $ term1 `TmApp` term2

-- ラムダ項パーサ
lambdaTermParse :: Parsec.Parsec String () LambdaTerm
lambdaTermParse
      = Parsec.try tmVarParse
    <|> Parsec.try tmAbsParse
    <|> Parsec.try tmAppParse

-- ラムダ項を表す文字列を LambdaTerm 型の値にパースする関数（名前は read 関数っぽいところから）
readLambdaTerm :: String -> Either Parsec.ParseError LambdaTerm
readLambdaTerm s = parse lambdaTermParse s

-- ラムダ項を表す文字列を LambdaTerm 型の値にパースする関数（エラー処理無しで値を直接返す）
parseLambdaTerm :: String -> LambdaTerm
parseLambdaTerm s = let Right t = parse lambdaTermParse s in t

