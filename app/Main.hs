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

getValue :: [(String, Tree)] -> String -> Tree
getValue vars name = fromMaybe (error $ "No variable named " ++ name) $ lookup name vars

evalExpression :: [(String, Tree)] -> Syntax -> Tree
evalExpression vars (Var name) = getValue vars name
evalExpression vars T = Leaf
evalExpression vars (Application a b) = f (evalExpression vars a) (evalExpression vars b)
evalExpression _ other = error $ "Unexpected syntax: " ++ show other

evalStatement :: [(String, Tree)] -> Syntax -> (String, Tree)
evalStatement vars (Assignment name [] expr) = (name, evalExpression vars expr)

evalStatements :: [(String, Tree)] -> [Syntax] -> [(String, Tree)]
evalStatements = foldl (\acc line -> evalStatement acc line : acc)

isStatement :: Syntax -> Bool
isStatement (Assignment _ _ _) = True
isStatement _ = False

isUsed :: String -> Syntax -> Bool
isUsed name (Lambda n s) = name /= n && isUsed name s
isUsed name (Application a b) = isUsed name a || isUsed name b
isUsed name (Var n) = name == n
isUsed name T = False

-- K = t t
_K = Application T T

-- I = t (t (t t)) t
_I = Application (Application T (Application T _K)) T

-- S = t (t (t t t)) t
_S = Application (Application T (Application T (Application _K T))) T

-- B = S (K S) K
_B = Application (Application _S (Application _K _S)) _K

-- C = S (B B S) (K K)
_C = Application (Application _S (Application (Application _B _B) _S)) (Application _K _K)

replaceLambdas :: Syntax -> Syntax
replaceLambdas (Assignment name (var : vars) tree) = replaceLambdas $ Assignment name vars (Lambda var tree)
replaceLambdas (Assignment name [] tree) = Assignment name [] $ replaceLambdas tree
replaceLambdas (Application a b) = Application (replaceLambdas a) (replaceLambdas b)
replaceLambdas (Lambda name tree) | not (isUsed name tree) = Application _K (replaceLambdas tree)
replaceLambdas (Lambda name T) = T
replaceLambdas (Lambda name (Var _)) = _I
replaceLambdas (Lambda name (Application f (Var n))) | name == n && not (isUsed name f) = f
replaceLambdas (Lambda name (Application f g))
  | isUsed name f && isUsed name g = Application (Application _S (replaceLambdas (Lambda name f))) (replaceLambdas (Lambda name g))
  | isUsed name f = Application (Application _C (replaceLambdas (Lambda name f))) g
  | isUsed name g = Application (Application _B f) (replaceLambdas (Lambda name g))
replaceLambdas (Lambda name (Lambda other e)) = replaceLambdas (Lambda name (replaceLambdas (Lambda other e)))
replaceLambdas tree = tree

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

data Syntax
  = Assignment String [String] Syntax
  | Lambda String Syntax
  | Application Syntax Syntax
  | Var String
  | T
  deriving (Show)

maybeF :: Maybe Syntax -> Syntax -> Syntax
maybeF Nothing s = s
maybeF (Just a) b = Application a b

parseHelper :: Maybe Syntax -> [Literal] -> (Syntax, [Literal])
parseHelper _ (Equals : _) = error "Unexpected equals sign"
parseHelper Nothing [] = error "Unexpected end of input"
parseHelper Nothing (EndParen : _) = error "Unexpected closing parenthesis"
parseHelper (Just syntax) [] = (syntax, [])
parseHelper (Just syntax) (EndParen : rest) = (syntax, rest)
parseHelper ms (Name "t" : rest) = parseHelper (Just $ maybeF ms T) rest
parseHelper ms (Name name : rest) = parseHelper (Just $ maybeF ms $ Var name) rest
parseHelper ms (Backslash : Name name : rest)
  | name == "t" = error "Cannot use reserved name t in lambda"
  | otherwise =
      let (innerSyntax, remaining) = parseHelper Nothing rest
       in parseHelper (Just $ Lambda name innerSyntax) remaining
parseHelper ms (StartParen : rest) =
  let (innerSyntax, remaining) = parseHelper Nothing rest
   in parseHelper (Just $ maybeF ms innerSyntax) remaining
parseHelper ms lits = error $ show lits

parse :: [Literal] -> Syntax
parse (Name name : lits)
  | Equals `elem` lits =
      let
        (vars, remaining) = span (/= Equals) lits
        names = reverse $ map (\(Name n) -> n) vars
       in
        case parseHelper Nothing (drop 1 remaining) of
          (syntax, []) -> replaceLambdas $ Assignment name names syntax
          (_, lits) -> error $ "Unexpected closing parenthesis before: " ++ show lits
parse lits = case parseHelper Nothing lits of
  (syntax, []) -> replaceLambdas syntax
  (_, lits) -> error $ "Unexpected closing parenthesis before: " ++ show lits

calculateSize :: Tree -> Integer
calculateSize Leaf = 0
calculateSize (Stem tree) = 1 + calculateSize tree
calculateSize (Branch l r) = 1 + (calculateSize l) + (calculateSize r)

main :: IO ()
main = do
  let filename = "example.txt"
  content <- readFile filename

  let allLines = map parse $ filter (/= []) $ map parseLiterals $ lines content
  let (statements, expressions) = partition isStatement allLines

  let vars = evalStatements [] statements
  let results = map (evalExpression vars) expressions
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
        outputStrLn ":syn <syntax> - show syntax tree"
        outputStrLn ":lit <literals> - show literals"
        loop vars
      Just (':' : 'b' : rest) -> do
        let rows :: [String] = map (\(name, tree) -> name ++ " = " ++ show tree) vars
        mapM_ outputStrLn rows
        loop vars
      Just (':' : 'l' : ' ' : rest) -> do
        let filename = trim rest
        result <- liftIO $ try $ readFile filename
        case result of
          Left (e :: SomeException) -> do
            outputStrLn $ "Error reading file: " ++ show e
            loop vars
          Right content -> do
            let allLines = map parse $ filter (/= []) $ map parseLiterals $ lines content
            let statements = filter isStatement allLines
            let newVars = evalStatements vars statements
            loop newVars
      Just (':' : 's' : rest) -> do
        let literals = parseLiterals rest
        let tree = parseExpression vars literals
        outputStrLn $ "Size: " ++ show (calculateSize tree)
      Just (':' : 's' : 'y' : 'n' : rest) -> do
        let literals = parseLiterals rest
        let syntax = parse literals
        outputStrLn $ show syntax
        loop vars
      Just (':' : 'l' : 'i' : 't' : rest) -> do
        outputStrLn . show $ parseLiterals rest
        loop vars
      Just input -> do
        let literals = parseLiterals input
        if null literals
          then loop vars
          else
            let syntax = parse literals
             in if isStatement syntax
                  then do
                    let (name, tree) = evalStatement vars syntax
                    loop ((name, tree) : vars)
                  else do
                    let tree = evalExpression vars syntax
                    result <- liftIO $ try $ print tree
                    case result of
                      Left (e :: SomeException) -> do
                        outputStrLn $ "Error evaluating: " ++ show e
                        loop vars
                      Right _ -> loop vars
