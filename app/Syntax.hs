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

-- Convenience function to not have to write Application everywhere
app :: [Syntax] -> Syntax
app (single : []) = single
app (first : second : rest) = app (Application first second : rest)

-- K = t t
_K :: Syntax
_K = app [T, T]

-- I = t (t K) t
_I :: Syntax
_I = app [T, app [T, _K], T]

-- S = t (t (t t t)) t
_S :: Syntax
_S = app [T, app [T, app [T, T, T]], T]

-- B = S (K S) K
_B :: Syntax
_B = app [_S, app [_K, _S], _K]

-- C = S (B B S) (K K)
_C :: Syntax
_C = app [_S, app [_B, _B, _S], app [_K, _K]]

-- U = S I I
_U :: Syntax
_U = app [_S, _I, _I]

-- Y = B U (C B U)
_Y :: Syntax
_Y = app [_B, _U, app [_C, _B, _U]]

-- Cases from Wikipedia: https://en.m.wikipedia.org/wiki/Combinatory_logic#Combinatory_calculi
replaceLambdas :: Syntax -> Syntax
replaceLambdas (Assignment name (var : vars) tree) = replaceLambdas $ Assignment name vars (Lambda var tree)
replaceLambdas (Assignment name [] tree)
  | isUsed name tree = Assignment name [] $ app [_Y, replaceLambdas (Lambda name tree)]
  | otherwise = Assignment name [] $ replaceLambdas tree
replaceLambdas (Application a b) = app [replaceLambdas a, replaceLambdas b] -- Case 2
replaceLambdas (Lambda name tree) | not (isUsed name tree) = app [_K, replaceLambdas tree] -- Case 3
replaceLambdas (Lambda name (Var _)) = _I -- Case 4
replaceLambdas (Lambda name (Lambda other e)) = replaceLambdas (Lambda name (replaceLambdas (Lambda other e))) -- Case 5
replaceLambdas (Lambda name (Application f (Var n))) | name == n && not (isUsed name f) = replaceLambdas f -- Eta
replaceLambdas (Lambda name (Application f g))
  | isUsed name f && isUsed name g =
      app [_S, replaceLambdas (Lambda name f), replaceLambdas (Lambda name g)] -- Case 6
  | isUsed name f = app [_C, replaceLambdas (Lambda name f), replaceLambdas g] -- Case 7
  | isUsed name g = app [_B, replaceLambdas f, replaceLambdas (Lambda name g)] -- Case 8
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
parseHelper (Just syntax) (Dollar : rest) =
  let (innerSyntax, remaining) = parseHelper Nothing rest
   in parseHelper (Just $ Application syntax innerSyntax) remaining
parseHelper ms (Name "t" : rest) = parseHelper (Just $ maybeF ms T) rest
parseHelper ms (Name name : rest) = parseHelper (Just $ maybeF ms $ Var name) rest
parseHelper ms (Number n : rest) = parseHelper (Just $ maybeF ms $ createNumber n) rest
parseHelper ms (Backslash : Name name : rest)
  | name == "t" = error "Cannot use reserved name t in lambda"
  | otherwise =
      let (innerSyntax, remaining) = parseHelper Nothing rest
       in parseHelper (Just $ maybeF ms $ Lambda name innerSyntax) remaining
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

_True :: Syntax
_True = T

_False :: Syntax
_False = app [T, T]

createNumber :: Integer -> Syntax
createNumber 0 = T
createNumber x =
  let (q, r) = x `quotRem` 2
   in app [T, if r == 0 then _False else _True, createNumber q]
