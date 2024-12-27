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

parseLine :: String -> Syntax
parseLine = parse . parseLiterals

mergeLines :: [String] -> [String]
mergeLines [] = []
mergeLines (line : otherLine@(c : cs) : rest)
  | isSpace c = mergeLines ((line ++ otherLine) : rest)
mergeLines (line : rest) = line : mergeLines rest

parseLines :: String -> ([Syntax], [Syntax])
parseLines = partition isStatement . map parse . filter (/= []) . map parseLiterals . mergeLines . lines

parseFile :: String -> IO ([Syntax], [Syntax])
parseFile filename = do
  result <- try $ readFile filename
  case result of
    Left (e :: SomeException) -> do
      print $ "Error reading file: " ++ show e
      return ([], [])
    Right content -> return $ parseLines content

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> runInputT defaultSettings (runFile filename [])
    [] -> runInputT defaultSettings (loop [])

runFile :: String -> [(String, Tree)] -> InputT IO ()
runFile fileName vars = do
  (statements, expressions) <- liftIO $ parseFile fileName
  runLines vars (statements, expressions)

runLines :: [(String, Tree)] -> ([Syntax], [Syntax]) -> InputT IO ()
runLines vars (statements, expressions) = do
  let newVars = evalStatements vars statements
  let results = map (evalExpression newVars) expressions

  printResult :: Either SomeException () <- liftIO . try $ mapM_ print results
  case printResult of
    Left (e :: SomeException) -> do
      outputStrLn $ "Error evaluating: " ++ show e
      loop vars
    Right () -> do
      loop newVars

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
      outputStrLn ":r <filename> - run file"
      outputStrLn ":s <expression> - size "
      outputStrLn ":n <expression> - show as number"
      outputStrLn ":syn <syntax> - show syntax tree"
      outputStrLn ":lit <literals> - show literals"
      loop vars
    Just (':' : 'b' : _) -> do
      let rows :: [String] = map (\(name, tree) -> name ++ " = " ++ show tree) vars
      mapM_ outputStrLn rows
      loop vars
    Just (':' : 'l' : ' ' : rest) -> do
      let filename = trim rest
      (statements, _) <- liftIO $ parseFile filename
      let newVars = evalStatements vars statements
      loop newVars
    Just (':' : 'r' : ' ' : rest) -> do
      let filename = trim rest
      runFile filename vars
    Just (':' : 's' : ' ' : rest) -> do
      let syntax = parseLine rest
      let tree = evalExpression vars syntax
      outputStrLn $ "Size: " ++ show (calculateSize tree)
      loop vars
    Just (':' : 'n' : ' ' : rest) -> do
      let syntax = parseLine rest
      let tree = evalExpression vars syntax
      outputStrLn $ "Number: " ++ show (asNumber tree)
      loop vars
    Just (':' : 's' : 'y' : 'n' : rest) -> do
      outputStrLn . show $ parseLine rest
      loop vars
    Just (':' : 'l' : 'i' : 't' : rest) -> do
      outputStrLn . show $ parseLiterals rest
      loop vars
    Just input -> do
      let (statements, expressions) = parseLines input
      runLines vars (statements, expressions)
