module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO()
main = do args <- getArgs
          putStrLn(readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!()-#" -- TODO: add more symbols

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
{-|
  "parse (spaces >> symbol) ": match spaces with first input, match symbol with rest of input

  correct
  "  jxls"
  "jlskj"
  ---
  incorrect
  "  jxls "
  "   "
  ""
-} 
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "not valid. wut u doin bru: " ++ show err
    Right val -> "valid symbol thk u"

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

{-|
  LispVal = algebraic data type
  Atom String = constructor, Atom = tag, String = type
  (proper) list: [a, b, c]
  dotted/improper list: (a b . c) 
-}
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

{-|
 - otherwise = binds atom variable to variable named otherwise. otherwise not a keyword, just readability.
-}
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> symbol <|> digit )
               let atom = [first] ++ rest
               return $ case atom of
                        "#t" -> Bool True 
                        "#f" -> Bool False 
                        otherwise -> Atom atom

{-|
 - string: " + any char 1 or more amount of times + "
 - return $ String x = return (String x)
 - $ = infix function application
-}

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ noneOf "\""
                 char '"'
                 return $ String x
                        
parseNumber :: Parser LispVal 
{-
 - liftM? need to place value into Parser monad
parseNumber = liftM (Number . read) $ many1 digit
-}
{- 
 - TODO: Use sequencing operator >>=
-}

parseNumber = do digits <- many1 digit
                 let number = read digits
                 return $ Number number

