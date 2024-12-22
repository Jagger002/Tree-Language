import System.IO


data Tree = Leaf | Stem Tree | Branch Tree Tree
 deriving (Show)
f:: Tree -> Tree -> Tree
f Leaf x = Stem x
f (Stem a)  x = Branch a x
f (Branch Leaf a) b = a
f (Branch (Stem a) b) c = f (f a c) (f b c)
f (Branch (Branch a b) c) Leaf = a
f (Branch (Branch a b) c) (Stem x) = f b x
f (Branch (Branch a b) c) (Branch x y) = f (f c x) y


ide :: Tree
ide = Branch (Stem (Stem Leaf)) Leaf

fste :: Tree
fste = Stem Leaf

snde :: Tree
snde = Branch Leaf ide


-- "t t"
-- Nothing Leaf, " t"
-- Leaf, " t"
-- Leaf "t"
-- Stem Leaf, ""
-- Stem Leaf

-- "t (t t) t"
-- Leaf, "(t t) t"
-- f Leaf (helper "t t"), " t"
-- Stem (Stem leaf), " t"
-- 


-- newtype Variables = Variables [(String, Tree)]

getValue :: [(String, Tree)] -> String -> Tree
getValue ((n,v):xs) name  | n == name = v
                          | otherwise = getValue xs name

parseExpression :: [(String, Tree)] -> [Literal] -> Tree
parseExpression vars s = let (_, tree) = helper Nothing s in tree
   where
      maybeF :: Maybe Tree -> Tree -> Tree
      maybeF Nothing x = x
      maybeF (Just x) y = f x y

      helper :: Maybe Tree -> [Literal] -> ([Literal], Tree)
      helper (Just tree) [] = ([], tree)
      helper (Nothing) [] = error "No thing in thing"
      helper (Just tree) (EndParen:xs) = (xs, tree)
      helper (Nothing) (EndParen:xs) = error "Wrong paren"
      helper tree (T:xs) = let newTree = maybeF tree Leaf in helper (Just newTree) xs
      helper tree (StartParen:xs) = let (remaingString, inreTree) = helper Nothing xs in helper (Just (maybeF tree inreTree)) remaingString
      helper tree (Var v:xs) = let newTree = maybeF tree (getValue vars v) in helper (Just newTree) xs 

                           
parseStatement :: [(String, Tree)] -> [Literal] -> (String, Tree)
parseStatement vars (Var name:Equals:expr) = (name, parseExpression vars expr)

parseStatements :: [(String, Tree)] -> [[Literal]] -> [(String, Tree)]
parseStatements vars [] = vars
parseStatements vars (line:lines) = let newVars = (parseStatement vars line):vars in parseStatements newVars lines

data Literal = Var String | T | Equals | StartParen | EndParen
 deriving Show

parseLiterals :: String -> [Literal]
parseLiterals ('(':xs) = StartParen:(parseLiterals xs)
parseLiterals (')':xs) = EndParen:(parseLiterals xs)
parseLiterals ('=':xs) = Equals:(parseLiterals xs)
parseLiterals (' ':xs) = parseLiterals xs
parseLiterals [] = []
parseLiterals s =
   let
      name = takeWhile (\c -> (c /= ' ') && (c /= ')')) s
      rest = drop (length name) s
   in (if (name == "t") then T else (Var name)):(parseLiterals rest)

main :: IO ()
main = do
    -- Ange filnamnet
    let filename = "example.txt"
    
    -- Läs innehållet i filen
    content <- readFile filename
    
    let allLines = lines content
    let exprLines = filter (\l -> l /= "") allLines 
    let statements = map parseLiterals $ filter (\l -> '=' `elem` l) exprLines
    let vars = parseStatements [] statements


    
    let expressions = map (\l -> parseExpression vars (parseLiterals l)) $ filter (\l -> not ('=' `elem` l)) exprLines
    mapM_ print expressions

    -- Skriv ut innehållet