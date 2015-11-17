import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Char
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
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

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

--parseCharacter :: Parser LispVal
--parseCharacter = do char "#\\"   

parseBool :: Parser LispVal
parseBool = do 
               char '#'
               x <- oneOf "tf"
               return $ case x of
                 't' -> Bool True
                 'f' -> Bool False

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
              return $ Atom atom

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


parseNumberEx2'4 :: Parser LispVal
parseNumberEx2'4 = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBinary

-- parseDecimal for default integers
parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit >>= (return . Number . read)

-- parseDecimal for explicit decimal values defined by #d
parseDecimal2 :: Parser LispVal
parseDecimal2 = do try $ string "#d"
                   x <- many1 digit
                   (return . Number . read) x 

-- parseHex to parse hexadecimal numbers
parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              (return . Number . hex2dec) x

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              (return . Number . oct2dec) x

parseBinary :: Parser LispVal
parseBinary = do try $ string "#b"
                 x <- many1 $ oneOf "01"
                 (return . Number . bin2dec) x

-- Reads a binary value and converts it to Decimal
type BinaryNum = String
bin2dec n = toInteger $ foldl (\acc x -> acc + (2^(snd x) * (digitToInt(fst x)))) 0 (zip (reverse n) naturalNums) 
    where naturalNums = iterate (+1) 0

toDecimal = fst . head
hex2dec = toDecimal . readHex
oct2dec = toDecimal . readOct

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumberEx2'4 <|> parseBool

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

main :: IO ()
main = do
         (expr:_) <- getArgs
         putStrLn (readExpr expr)
