module Lexer where
import Text.Read (readMaybe)

data Literal = Name String | Equals | StartParen | EndParen | Backslash | Dollar | Number Integer
  deriving (Show, Eq)

parseLiterals :: String -> [Literal]
parseLiterals ('#' : xs) = []
parseLiterals ('(' : xs) = StartParen : parseLiterals xs
parseLiterals (')' : xs) = EndParen : parseLiterals xs
parseLiterals ('=' : xs) = Equals : parseLiterals xs
parseLiterals ('\\' : xs) = Backslash : parseLiterals xs
parseLiterals ('$' : xs) = Dollar : parseLiterals xs
parseLiterals (' ' : xs) = parseLiterals xs
parseLiterals [] = []
parseLiterals s =
  let (token, rest) = span (\c -> c /= ' ' && c /= ')') s
   in case readMaybe token :: Maybe Integer of
        Just num -> Number num : parseLiterals rest
        Nothing  -> Name token : parseLiterals rest
