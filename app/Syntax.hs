module Syntax (Syntax (Assignment, Lambda, Application, Var, T), parse, isStatement) where

import Lexer

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

isStatement :: Syntax -> Bool
isStatement (Assignment _ _ _) = True
isStatement _ = False

isUsed :: String -> Syntax -> Bool
isUsed name (Lambda n s) = name /= n && isUsed name s
isUsed name (Application a b) = isUsed name a || isUsed name b
isUsed name (Var n) = name == n
isUsed name T = False

-- K = t t
_K :: Syntax
_K = Application T T

-- I = t (t (t t)) t
_I :: Syntax
_I = Application (Application T (Application T _K)) T

-- S = t (t (t t t)) t
_S :: Syntax
_S = Application (Application T (Application T (Application _K T))) T

-- B = S (K S) K
_B :: Syntax
_B = Application (Application _S (Application _K _S)) _K

-- C = S (B B S) (K K)
_C :: Syntax
_C = Application (Application _S (Application (Application _B _B) _S)) (Application _K _K)

-- Cases from Wikipedia: https://en.m.wikipedia.org/wiki/Combinatory_logic#Combinatory_calculi
replaceLambdas :: Syntax -> Syntax
replaceLambdas (Assignment name (var : vars) tree) = replaceLambdas $ Assignment name vars (Lambda var tree)
replaceLambdas (Assignment name [] tree) = Assignment name [] $ replaceLambdas tree
replaceLambdas (Application a b) = Application (replaceLambdas a) (replaceLambdas b) -- Case 2
replaceLambdas (Lambda name tree) | not (isUsed name tree) = Application _K (replaceLambdas tree) -- Case 3
replaceLambdas (Lambda name (Var _)) = _I -- Case 4
replaceLambdas (Lambda name (Lambda other e)) = replaceLambdas (Lambda name (replaceLambdas (Lambda other e))) -- Case 5
replaceLambdas (Lambda name (Application f (Var n))) | name == n && not (isUsed name f) = f -- Eta
replaceLambdas (Lambda name (Application f g))
  | isUsed name f && isUsed name g =
      Application (Application _S (replaceLambdas (Lambda name f))) (replaceLambdas (Lambda name g)) -- Case 6
  | isUsed name f = Application (Application _C (replaceLambdas (Lambda name f))) (replaceLambdas g) -- Case 7
  | isUsed name g = Application (Application _B (replaceLambdas f)) (replaceLambdas (Lambda name g)) -- Case 8
replaceLambdas tree = tree -- Case 1

-- Splits the string until the next matching closing parenthesis.
-- example: "t (t t) t) t" -> ("t (t t) t" "t")
insideParens :: [Literal] -> ([Literal], [Literal])
insideParens (EndParen : rest) = ([], rest)
insideParens (StartParen : rest) =
  let (innerLits, remaining) = insideParens rest
      (left, right) = insideParens remaining
   in (StartParen : innerLits ++ EndParen : left, right)
insideParens (lit : rest) =
  let (innerLits, remaining) = insideParens rest
   in (lit : innerLits, remaining)

parseHelper :: Maybe Syntax -> [Literal] -> (Syntax, [Literal])
parseHelper _ (Equals : _) = error "Unexpected equals sign"
parseHelper Nothing [] = error "Unexpected end of input"
parseHelper _ (EndParen : _) = error "Unexpected closing parenthesis"
parseHelper (Just syntax) [] = (syntax, [])
parseHelper ms (Name "t" : rest) = parseHelper (Just $ maybeF ms T) rest
parseHelper ms (Name name : rest) = parseHelper (Just $ maybeF ms $ Var name) rest
parseHelper ms (Backslash : Name name : rest)
  | name == "t" = error "Cannot use reserved name t in lambda"
  | otherwise =
      let (innerSyntax, remaining) = parseHelper Nothing rest
       in parseHelper (Just $ Lambda name innerSyntax) remaining
parseHelper ms (StartParen : rest) =
  let (insidePars, rightOfPars) = insideParens rest
      (innerSyntax, []) = parseHelper Nothing insidePars
   in parseHelper (Just $ maybeF ms innerSyntax) rightOfPars
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
