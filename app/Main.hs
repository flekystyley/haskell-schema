module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol::Parser Char
-- oneOf - 引数にとった文字列が、指定された文字リストかどうか判断し解析
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr::String -> String

-- parse <パーサー> <名前> <パースしたい文字列>
-- >>(バインド記号)で2つの関数を組み合わせる
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
    (expr :_) <- getArgs
    putStrLn(readExpr expr)
