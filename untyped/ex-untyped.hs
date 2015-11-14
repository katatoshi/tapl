{-# LANGUAGE FlexibleContexts #-}

import Lambda

import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)

import Control.Monad.Writer
import qualified Data.List.Utils as Utils
import qualified Data.Map as Map
import System.IO

-- 項の名前付け拡張
--
-- BNF
--
-- lower ::= 'a' | ... | 'z'
--
-- termName ::= lower lower
--            | termName lower
--
-- exTerm ::= term
--          | termName
--          | '(' '\\' varName '.' exTerm ')'
--          | '(' exTerm ' ' exTerm ')'
--          | termName '=' exTerm
--
-- 拡張された項の termName をすべて term に展開して、
-- 型無しラムダ計算の項に直してから、パースと評価を行う。
-- したがって、型無しラムダ計算の体系そのものは拡張していない。
--

type TermName = String
type ExString = String
type TermMap = Map.Map TermName String

-- parse 関数 - http://unbui.lt/#!/post/haskell-parsec-basics
parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule text = Parsec.parse rule "" text

expandOneName :: TermName -> ExString -> ExString -> ExString
expandOneName = Utils.replace

expandAllName :: TermMap -> ExString -> String
expandAllName map exTerm = Map.foldrWithKey expandOneName exTerm map

nameTerm :: TermName -> ExString -> TermMap -> TermMap
nameTerm name exTerm map = Map.insert name expandedTerm map
    where expandedTerm = expandAllName map exTerm

-- 項の名前パーサ
termNameParse :: Parsec.Parsec String () String
termNameParse = do
    lower <- Parsec.lower
    lowers <- Parsec.many1 Parsec.lower
    return $ (lower : lowers)

-- 項の名前付けパーサ
nameTermParse :: TermMap -> Parsec.Parsec String () (Either TermMap (TermMap, String))
nameTermParse map = do
    name <- termNameParse
    Parsec.spaces
    Parsec.char '='
    Parsec.spaces
    exTerm <- Parsec.many Parsec.anyChar
    return $ Left $ nameTerm name exTerm map

-- 名前を展開した項を結果として返すだけのルール
expandNameParse :: TermMap -> Parsec.Parsec String () (Either TermMap (TermMap, String))
expandNameParse map = do
    exTerm <- Parsec.many Parsec.anyChar
    return $ Right (map, expandAllName map exTerm)



main = mainInteractive $ Map.empty

-- 評価回数の上限
maxEvalStep :: Int
maxEvalStep = 50

-- ラムダ項の文字列を受け取り、パースしたものの評価ステップを標準出力に出力する関数
-- パースに失敗した場合はエラー内容を表示する
printEvalStep' :: String -> IO ()
printEvalStep' s = case parseLambdaTerm s of
    Right t -> mapM_ putStrLn $ take maxEvalStep $ snd $ runWriter $ evalWithLog t
    Left e -> print e

-- 拡張されたラムダ項の文字列を項の名前のマップまたは項の名前のマップとラムダ項の文字列の組を返す関数
parseExtendedLambdaTerm :: TermMap -> String -> Either Parsec.ParseError (Either TermMap (TermMap, String))
parseExtendedLambdaTerm map = parse (Parsec.try (nameTermParse map) <|> (expandNameParse map))

mainInteractive :: TermMap -> IO ()
mainInteractive map = do
    putStr "ex-untyped> "
    hFlush stdout
    s <- getLine
    putStrLn $ "input: " ++ s
    r <- return $ parseExtendedLambdaTerm map s
    h <- getExResult map r
    map' <- workAndReturnNewMap h
    mainInteractive map'

getExResult :: TermMap -> (Either Parsec.ParseError (Either TermMap (TermMap, String))) -> IO (Either TermMap (TermMap, String))
getExResult map (Right r) = return $ r
getExResult map (Left e) = do
    print e
    return $ Left map

workAndReturnNewMap :: Either TermMap (TermMap, String) -> IO TermMap
workAndReturnNewMap (Right (map, s)) = do
    putStrLn "evaluation steps:"
    printEvalStep' s
    putStrLn ""
    return map
workAndReturnNewMap (Left map) = do 
    putStrLn "create new map!"
    putStrLn ""
    return map

