module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"
spaces :: Parser ()
spaces = skipMany1 space

escapeChars :: Parser Char
escapeChars = do char '\\'
                 x <- oneOf ("\\\"rnt")
                 return $ x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value "

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (escapeChars <|> noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of 
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

parseNumber' :: Parser LispVal
parseNumber' = liftM (Number . read) $ many1 digit

parseNum :: Parser LispVal
parseNum = do retval <- many1 digit
              return $ (Number . read) retval

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= 
           \x -> return $ (Number . read)  x

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseNumber
        <|> parseString

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))
