module Lexer where

data Literal = Name String | Equals | StartParen | EndParen | Backslash
  deriving (Show, Eq)

parseLiterals :: String -> [Literal]
parseLiterals ('#' : xs) = []
parseLiterals ('(' : xs) = StartParen : parseLiterals xs
parseLiterals (')' : xs) = EndParen : parseLiterals xs
parseLiterals ('=' : xs) = Equals : parseLiterals xs
parseLiterals ('\\' : xs) = Backslash : parseLiterals xs
parseLiterals (' ' : xs) = parseLiterals xs
parseLiterals [] = []
parseLiterals s =
  let (name, rest) = span (\c -> c /= ' ' && c /= ')') s
   in (Name name) : parseLiterals rest
