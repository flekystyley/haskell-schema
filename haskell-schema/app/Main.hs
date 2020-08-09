module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

-- 型を定義する
symbol::Parser Char
-- oneOf - 引数にとった文字列が、指定された文字リストかどうか判断し解析します
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr::String -> String

-- parser関数は parse <パーサー> <名前> <パースしたい文字列>
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match" ++ show err
    Right val -> "Found Value"

main :: IO ()
main = do
    (expr :_) <- getArgs
    putStrLn(readExpr expr)
