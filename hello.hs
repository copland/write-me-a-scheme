module Main where
import System.Environment

main :: IO ()
main = do
    args <- getLine
    let result = (read  (args !! 0))
    putStrLn (show result)
