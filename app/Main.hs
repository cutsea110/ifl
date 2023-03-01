module Main where

import Control.Monad (forM_)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (getContents, hPutStr, hPutStrLn, stdout, stderr)

import qualified Template.Mark1 as Mark1 (runProg)
import qualified Template.Mark2 as Mark2 (runProg)
import qualified Template.Mark3 as Mark3 (runProg)
import qualified Template.Mark4 as Mark4 (runProg)
import qualified Template.Mark5 as Mark5 (runProg, runProgWithConv)
import qualified Template.Mark5Alt as Mark5Alt (runProg, runProgWithConv)
import qualified Template.Mark5GC as Mark5GC (runProg, runProgWithConv)
import qualified Template.Mark5RevGC as Mark5RevGC (runProg, runProgWithConv)
import qualified Template.Mark5Cp as Mark5Cp (runProg, runProgWithConv)

import qualified Gmachine.Mark1 as GMark1 (runProg)
import qualified Gmachine.Mark2 as GMark2 (runProg)
import qualified Gmachine.Mark3 as GMark3 (runProg)
import qualified Gmachine.Mark4 as GMark4 (runProg)
import qualified Gmachine.Mark5 as GMark5 (runProg)
import qualified Gmachine.Mark6 as GMark6 (runProg)


---------------------------------------------------------------
-- COMPILER
---------------------------------------------------------------
type Executer = String -> IO ()

executer :: Compiler -> Executer
executer Mark1 = putStrLn . Mark1.runProg
executer Mark2 = putStrLn . Mark2.runProg
executer Mark3 = putStrLn . Mark3.runProg
executer Mark4 = putStrLn . Mark4.runProg
executer Mark5 = putStrLn . Mark5.runProg
executer Mark5cnv = putStrLn . Mark5.runProgWithConv
executer Mark5Alt = putStrLn . Mark5Alt.runProg
executer Mark5Altcnv = putStrLn . Mark5Alt.runProgWithConv
executer Mark5GC = putStrLn . Mark5GC.runProg
executer Mark5GCcnv = putStrLn . Mark5GC.runProgWithConv
executer Mark5RevGC = putStrLn . Mark5RevGC.runProg
executer Mark5RevGCcnv = putStrLn . Mark5RevGC.runProgWithConv
executer Mark5Cp = putStrLn . Mark5Cp.runProg
executer Mark5Cpcnv = putStrLn . Mark5Cp.runProgWithConv
executer GMark1 = putStrLn . GMark1.runProg
executer GMark2 = putStrLn . GMark2.runProg
executer GMark3 = putStrLn . GMark3.runProg
executer GMark4 = putStrLn . GMark4.runProg
executer GMark5 = putStrLn . GMark5.runProg
executer GMark6 = putStrLn . GMark6.runProg
executer (Noco name) = \_ -> do
  putStrLn $ "Error: Unknown compiler = " ++ name
  printHelp

---------------------------------------------------------------
-- COMMAND LINE OPTIONS
---------------------------------------------------------------

data Compiler = Noco String
              | Mark1
              | Mark2
              | Mark3
              | Mark4
              | Mark5
              | Mark5cnv
              | Mark5Alt
              | Mark5Altcnv
              | Mark5GC
              | Mark5GCcnv
              | Mark5RevGC
              | Mark5RevGCcnv
              | Mark5Cp
              | Mark5Cpcnv
              | GMark1
              | GMark2
              | GMark3
              | GMark4
              | GMark5
              | GMark6
              deriving Show

data Options = Options
  { optVerbose     :: Bool -- TODO
  , optShowVersion :: Bool
  , optCompiler    :: Compiler
  }

defaultOptions :: Options
defaultOptions = Options
  { optVerbose     = False
  , optShowVersion = False
  , optCompiler    = GMark6
  }

name2Compiler :: [(String, Compiler)]
name2Compiler = [ ("mark1", Mark1)
                , ("mark2", Mark2)
                , ("mark3", Mark3)
                , ("mark4", Mark4)
                , ("mark5", Mark5)
                , ("mark5cnv", Mark5cnv)           -- convert newer version
                , ("mark5alt", Mark5Alt)
                , ("mark5altcnv", Mark5Altcnv)     -- convert newer version
                , ("mark5gc", Mark5GC)             -- mark gc
                , ("mark5gccnv", Mark5GCcnv)       -- convert newer version
                , ("mark5revgc", Mark5RevGC)       -- pointer reversal mark gc
                , ("mark5revgccnv", Mark5RevGCcnv) -- convert newer version
                , ("mark5cp", Mark5Cp)             -- convert newer version
                , ("gmark1", GMark1)               -- Gmachine
                , ("gmark2", GMark2)               -- Gmachine
                , ("gmark3", GMark3)               -- Gmachine
                , ("gmark4", GMark4)               -- Gmachine
                , ("gmark5", GMark5)               -- Gmachine
                , ("gmark6", GMark6)               -- Gmachine
                ]

compilerNames :: [String]
compilerNames = map fst name2Compiler

options :: [OptDescr (Options -> Options)]
options = [ Option ['c']      ["compiler"]  (ReqArg (\e opts -> opts {optCompiler = decide e}) "Compiler")
            ("compiler name (" ++ intercalate " | " compilerNames ++ ")")
          , Option ['v']      ["verbose"]   (NoArg (\opts -> opts {optVerbose = True}))
            "chatty output on stderr"
          , Option ['V', '?'] ["version"]   (NoArg (\opts -> opts {optShowVersion = True}))
            "show version"
          ]
  where decide :: String -> Compiler
        decide name = fromMaybe (Noco name) $ lookup name name2Compiler
          
compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: cabal v2-run ifl -- [OPTION...] <program-file>"

---------------------------------------------------------------
-- MAIN
---------------------------------------------------------------

run :: Options -> FilePath -> IO ()
run opts fp = do
  hPutStrLn stderr $ "Program Source: " ++ fp
  hPutStrLn stderr $
    "The compilers that can be specified are as follows: " ++ intercalate "," compilerNames ++ "."
  executer (optCompiler opts) =<< readFile fp

printHelp :: IO ()
printHelp = do
  -- This banner generated by using 'figlet -f slant IFL'
  hPutStr stderr $ unlines [ "    ____________"
                           , "   /  _/ ____/ /"
                           , "   / // /_  / /"
                           , " _/ // __/ / /___"
                           , "/___/_/   /_____/ Implimenting Functional Languages"
                           , ""
                           , "> cabal v2-run ifl -- [OPTION...] <program-file>"
                           , usageInfo "OPTION" options
                           ]

main :: IO ()
main = do
  args <- getArgs
  (opts, rest) <- compilerOpts args
  if null rest then printHelp
    else forM_ rest (run opts)
