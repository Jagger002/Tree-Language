{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.List (isPrefixOf, lookup, partition)
import Data.Maybe (fromMaybe)
import System.Console.Haskeline
import System.IO

data Tree = Leaf | Stem Tree | Branch Tree Tree
  deriving (Show)

f :: Tree -> Tree -> Tree
f Leaf x = Stem x
f (Stem a) x = Branch a x
f (Branch Leaf a) b = a
f (Branch (Stem a) b) c = f (f a c) (f b c)
f (Branch (Branch a b) c) Leaf = a
f (Branch (Branch a b) c) (Stem x) = f b x
f (Branch (Branch a b) c) (Branch x y) = f (f c x) y

-- newtype Variables = Variables [(String, Tree)]

getValue :: [(String, Tree)] -> String -> Tree
getValue vars name = fromMaybe (error "No variable with that name") $ lookup name vars

parseExpression :: [(String, Tree)] -> [Literal] -> Tree
parseExpression vars s = let (_, tree) = helper Nothing s in tree
 where
  maybeF :: Maybe Tree -> Tree -> Tree
  maybeF Nothing x = x
  maybeF (Just x) y = f x y

  helper :: Maybe Tree -> [Literal] -> ([Literal], Tree)
  helper (Just tree) [] = ([], tree)
  helper Nothing [] = error "Unexpected end of input"
  helper (Just tree) (EndParen : xs) = (xs, tree)
  helper Nothing (EndParen : xs) = error "Unexpected closing parenthesis"
  helper tree (T : xs) = let newTree = maybeF tree Leaf in helper (Just newTree) xs
  helper tree (StartParen : xs) = let (remaingString, inreTree) = helper Nothing xs in helper (Just (maybeF tree inreTree)) remaingString
  helper tree (Var v : xs) = let newTree = maybeF tree (getValue vars v) in helper (Just newTree) xs

parseStatement :: [(String, Tree)] -> [Literal] -> (String, Tree)
parseStatement vars (Var name : Equals : expr) = (name, parseExpression vars expr)

parseStatements :: [(String, Tree)] -> [[Literal]] -> [(String, Tree)]
parseStatements = foldl (\acc line -> parseStatement acc line : acc)

isStatement :: [Literal] -> Bool
isStatement (Var _ : Equals : _) = True
isStatement _ = False

data Literal = Var String | T | Equals | StartParen | EndParen
  deriving (Show, Eq)

parseLiterals :: String -> [Literal]
parseLiterals ('#' : xs) = []
parseLiterals ('(' : xs) = StartParen : parseLiterals xs
parseLiterals (')' : xs) = EndParen : parseLiterals xs
parseLiterals ('=' : xs) = Equals : parseLiterals xs
parseLiterals (' ' : xs) = parseLiterals xs
parseLiterals [] = []
parseLiterals s =
  let
    (name, rest) = span (\c -> c /= ' ' && c /= ')') s
    val = if name == "t" then T else Var name
   in
    val : parseLiterals rest

calculateSize :: Tree -> Integer
calculateSize Leaf = 0
calculateSize (Stem tree) = 1 + calculateSize tree
calculateSize (Branch l r) = 1 + (calculateSize l) + (calculateSize r)

main :: IO ()
main = do
  let filename = "example.txt"
  content <- readFile filename

  let allLines = filter (/= []) $ map parseLiterals $ lines content
  let (statements, expressions) = partition isStatement allLines

  let vars = parseStatements [] statements
  let results = map (parseExpression vars) expressions
  mapM_ print results

trim :: String -> String
trim = f . f
 where
  f = reverse . dropWhile isSpace

repl :: IO ()
repl = runInputT defaultSettings (loop [])
 where
  loop :: [(String, Tree)] -> InputT IO ()
  loop vars = do
    minput <- getInputLine "% "
    case minput of
      Nothing -> return ()
      Just ":q" -> return ()
      Just ":?" -> do
        outputStrLn "Commands:"
        outputStrLn ":q - quit"
        outputStrLn ":? - help"
        outputStrLn ":b - list variables"
        outputStrLn ":l <filename> - load file"
        outputStrLn ":s <expression> - size "
        loop vars
      Just (':' : 'b' : rest) -> do
        let rows :: [String] = map (\(name, tree) -> name ++ " = " ++ show tree) vars
        mapM_ outputStrLn rows
        loop vars
      Just (':' : 'l' : rest) -> do
        let filename = trim rest
        result <- liftIO $ try $ readFile filename
        case result of
          Left (e :: SomeException) -> do
            outputStrLn $ "Error reading file: " ++ show e
            loop vars
          Right content -> do
            let allLines = filter (/= []) $ map parseLiterals $ lines content
            let statements = filter isStatement allLines
            let newVars = parseStatements vars statements
            loop newVars
      Just (':' : 's' : rest) -> do
        let literals = parseLiterals rest
        let tree = parseExpression vars literals
        outputStrLn $ "Size: " ++ show (calculateSize tree)
        loop vars
      Just input -> do
        let literals = parseLiterals input
        case literals of
          [] -> loop vars
          _ | isStatement literals -> do
            let (name, tree) = parseStatement vars literals
            loop ((name, tree) : vars)
          _ -> do
            let tree = parseExpression vars literals
            result <- liftIO $ try $ print tree
            case result of
              Left (e :: SomeException) -> do
                outputStrLn $ "Error evaluating: " ++ show e
                loop vars
              Right _ -> loop vars
