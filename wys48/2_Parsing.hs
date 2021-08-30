module Main where
import Control.Monad
import System.Environment

import Text.ParserCombinators.Parsec hiding (spaces)

-- class  Monad m  where
--     (>>=)            :: m a -> (a -> m b) -> m b
--     (>>)             :: m a -> m b -> m b
--     return           :: a -> m a
--     fail             :: String -> m a

-- m >> k           =  m >>= \_ -> k


data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\\\"nrt"
                  return $ case x of
                    '"'  -> x
                    '\\' -> x
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'


parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChars <|> noneOf "\"\\"
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                 "#t" -> Bool True
                 "#f" -> Bool False
                 _    -> Atom atom


-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- Exercice 1
-- Rewrite parseNumber, without liftM, using
--    do-notation
--    explicit sequencing with the >>= operator

-- parseNumber :: Parser LispVal
-- parseNumber = do nb <- many1 digit
--                  (return . Number . read) nb

-- parseNumber :: Parser LispVal
-- parseNumber = many1 digit >>= return . Number . read

-- END Exercice 1

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ input


main :: IO ()
main = do (expr:_) <- getArgs
          putStrLn (readExpr expr)
