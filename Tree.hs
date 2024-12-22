import Data.List (lookup, partition)
import Data.Maybe (fromMaybe)
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

data Literal = Var String | T | Equals | StartParen | EndParen
  deriving (Show, Eq)

parseLiterals :: String -> [Literal]
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

main :: IO ()
main = do
  let filename = "example.txt"
  content <- readFile filename

  let allLines = filter (/= []) $ map parseLiterals $ lines content
  let (statements, expressions) = partition (elem Equals) allLines

  let vars = parseStatements [] statements
  let results = map (parseExpression vars) expressions
  mapM_ print results