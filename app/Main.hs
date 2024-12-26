module Main where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.List (partition)
import System.Console.Haskeline
import System.Environment (getArgs)

import Eval
import Lexer
import Syntax
import Tree

trim :: String -> String
trim = f . f
 where
  f = reverse . dropWhile isSpace

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename

      let allLines = map parse $ filter (/= []) $ map parseLiterals $ lines content
      let (statements, expressions) = partition isStatement allLines

      let vars = evalStatements [] statements
      let results = map (evalExpression vars) expressions
      mapM_ print results
    [] -> repl

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
        outputStrLn ":l <filename> - load file variables"
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
      Just (':' : 's' : ' ' : rest) -> do
        let literals = parseLiterals rest
        let syntax = parse literals
        let tree = evalExpression vars syntax
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
