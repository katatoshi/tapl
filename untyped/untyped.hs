import Lambda
import Control.Monad.Writer
import System.IO

--
-- $ ghc --make untyped.hs
-- [1 of 3] Compiling LambdaTerm       ( LambdaTerm.hs, LambdaTerm.o )
-- [2 of 3] Compiling Lambda           ( Lambda.hs, Lambda.o )
-- [3 of 3] Compiling Main             ( untyped.hs, untyped.o )
-- Linking untyped ...
-- $ ./untyped
-- untyped> ((\x.x) (\z.((\x.x) z)))
-- input: ((\x.x) (\z.((\x.x) z)))
-- evaluation steps:
-- ((\x.x) (\z.((\x.x) z)))
-- (\z.((\x.x) z))
--
-- untyped> (\x.
-- input: (\x.
-- evaluation steps:
-- (line 1, column 5):
-- unexpected end of input
-- expecting lowercase letter or "("
--
-- またはファイルからリダイレクト
--
-- $ ./untyped < test.txt
--
-- test.txt の後ろ 4 つは TaPL pp.44-45 の Church ブール値 tru, fls, test の評価例
-- ometa.txt は p.49 の発散コンビネータの評価例（ステップは maxEvalStep だけ表示）
--

main = forever $ do
    putStr "untyped> "
    hFlush stdout
    s <- getLine
    putStrLn $ "input: " ++ s
    putStrLn "evaluation steps:"
    printEvalStep' s
    putStrLn ""

-- 評価回数の上限
maxEvalStep :: Int
maxEvalStep = 50

-- ラムダ項の文字列を受け取り、パースしたものの評価ステップを標準出力に出力する関数
-- パースに失敗した場合はエラー内容を表示する
printEvalStep' :: String -> IO ()
printEvalStep' s = case parseLambdaTerm s of
    Right t -> mapM_ putStrLn $ take maxEvalStep $ snd $ runWriter $ evalWithLog t
    Left e -> print e

