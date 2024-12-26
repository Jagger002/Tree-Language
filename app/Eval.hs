module Eval where

import Data.Maybe (fromMaybe)

import Syntax
import Tree

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
