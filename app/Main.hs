module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

symbol::Parser Char
-- oneOf - 引数にとった文字列が、指定された文字リストかどうか判断し解析
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces::Parser()
spaces = skipMany1 space

data LispVal = Atom String
    | List[LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool

parseString::Parser LispVal
parseString = do
        char '"'
        x <- many (noneOf "\"")
        char '"'
        return $String x

parseAtom::Parser LispVal
parseAtom = do
        first <- letter <|> symbol
        rest <- many (letter <|> digit <|> symbol)
        let atom = first:rest
        return $case atom of
            "#t" -> Bool True
            "#f" -> Bool False
            _    -> Atom atom

parseNumber::Parser LispVal
parseNumber = liftM (Number .read) $ many1 digit

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
