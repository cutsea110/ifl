module Main where

import System.Environment (getArgs)
import System.IO (getContents)

-- import Template.Mark1 (parse, compile, eval, showResults)
-- import Template.Mark2 (parse, compile, eval, showResults)
-- import Template.Mark3 (parse, compile, eval, showResults)
import Template.Mark4 (parse, compile, eval, showResults)

run :: String -> IO ()
run = putStrLn . showResults . eval . compile . parse

printHelp :: IO ()
printHelp = putStrLn "cabal v2-run ifl <file-path>"

exec :: FilePath -> IO ()
exec file = do
  putStrLn $ "Program Source: " ++ file
  testProg <- readFile file
  -- compile and eval program
  run testProg

main :: IO ()
main = do
  args <- getArgs
  if null args then printHelp
    else exec (head args)
