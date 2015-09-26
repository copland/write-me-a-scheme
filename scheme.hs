import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.Environment

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Chararacter Char

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\\\"nrt"
                  return $ case x of
                    '\\' -> x
                    '"'  -> x
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'

parseCharacter :: Parser LispVal
parseCharacter    

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

parseStringEx2 :: Parser LispVal
parseStringEx2 = do
                   char '"'
                   x <- many $ escapedChars <|> noneOf "\"\\"
                   char '"'
                   return $ String x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

-- parseNumber using liftM
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- parseNumber using do-notation
parseNumberEx1'1 :: Parser LispVal
parseNumberEx1'1 = do
                 number <- many1 digit
                 return $ (Number . read) number

-- parseNumber using bind (>>=)
parseNumberEx1'2 :: Parser LispVal
parseNumberEx1'2 = many1 digit >>= \n ->
                   return $ (Number . read) n

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumberEx1'2

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

main :: IO ()
main = do
         (expr:_) <- getArgs
         putStrLn (readExpr expr)
