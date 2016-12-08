module Main where

import Syntax
import Interpret

main :: IO ()
main = do
    file <- readFile "example.bf"
    case parse file of
         Left err -> showParseError err
         Right bf -> execute_ bf
