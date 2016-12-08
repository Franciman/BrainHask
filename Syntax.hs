module Syntax
    ( Instruction(..)
    , SyntaxTree
    , ParseError
    , showParseError
    , parse
    ) where

import Control.Arrow (first)

data Instruction = IncrementPointer            -- >
                 | DecrementPointer            -- <
                 | IncrementByte               -- +
                 | DecrementByte               -- -
                 | Output                      -- .
                 | Input                       -- ,
                 | Squared [Instruction]       -- [ ... ]
                 deriving (Show)

type SyntaxTree = [Instruction]

type ParseError = String

showParseError :: ParseError -> IO ()
showParseError = putStrLn

parse :: String -> Either ParseError SyntaxTree
parse str = fst <$> parse' 0 str

parse' :: Int -> String -> Either ParseError (SyntaxTree, String)
parse' 0 [] = Right ([], [])
parse' depth [] = Left "Error: Unbalanced square brackets"
parse' depth (x:xs) = case x of
                           '>' -> continueParsing IncrementPointer xs

                           '<' -> continueParsing DecrementPointer xs

                           '+' -> continueParsing IncrementByte xs

                           '-' -> continueParsing DecrementByte xs

                           '.' -> continueParsing Output xs

                           ',' -> continueParsing Input xs

                           '[' -> uncurry continueParsing . first Squared =<< parse' (depth + 1) xs

                           ']' -> if depth == 0
                                    then Left "Error: Unbalanced square brackets"
                                    else Right ([], xs)

                           _   -> parse' depth xs


    where continueParsing :: Instruction -> String -> Either ParseError (SyntaxTree, String)
          continueParsing instr cont = first (instr :) <$> parse' depth cont
