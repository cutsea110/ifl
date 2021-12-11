module Main where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (getContents, hPutStr, hPutStrLn, stdout, stderr)

import qualified Template.Mark1 as Mark1 (parse, compile, eval, showResults)
import qualified Template.Mark2 as Mark2 (parse, compile, eval, showResults)
import qualified Template.Mark3 as Mark3 (parse, compile, eval, showResults)
import qualified Template.Mark4 as Mark4 (parse, compile, eval, showResults)

---------------------------------------------------------------
-- COMPILER
---------------------------------------------------------------
type Executer = String -> IO ()

executer :: Compiler -> Executer
executer Mark1 = putStrLn . Mark1.showResults . Mark1.eval . Mark1.compile . Mark1.parse
executer Mark2 = putStrLn . Mark2.showResults . Mark2.eval . Mark2.compile . Mark2.parse
executer Mark3 = putStrLn . Mark3.showResults . Mark3.eval . Mark3.compile . Mark3.parse
executer Mark4 = putStrLn . Mark4.showResults . Mark4.eval . Mark4.compile . Mark4.parse
executer (Noco name) _ = do
  putStrLn $ "Error: Unknown compiler engine = " ++ name
  printHelp

---------------------------------------------------------------
-- COMMAND LINE OPTIONS
---------------------------------------------------------------

data Compiler = Noco String | Mark1 | Mark2 | Mark3 | Mark4 deriving Show

data Options = Options
  { optVerbose     :: Bool -- TODO
  , optShowVersion :: Bool
  , optEngine      :: Compiler
  }

defaultOptions :: Options
defaultOptions = Options
  { optVerbose     = True  -- TODO
  , optShowVersion = False
  , optEngine      = Mark4
  }

name2Compiler :: [(String, Compiler)]
name2Compiler = [ ("mark1", Mark1)
                , ("mark2", Mark2)
                , ("mark3", Mark3)
                , ("mark4", Mark4)
                ]

options :: [OptDescr (Options -> Options)]
options = [ Option ['v']      ["verbose"]   (NoArg (\opts -> opts {optVerbose = True}))
            "chatty output on stderr"
          , Option ['V', '?'] ["version"]   (NoArg (\opts -> opts {optShowVersion = True}))
            "show version"
          , Option ['c']      ["compiler"]  (ReqArg (\e opts -> opts {optEngine = decideEngine e}) "Compiler")
            ("compiler name [" ++ intercalate "|" compilers ++ "]")
          ]
  where decideEngine :: String -> Compiler
        decideEngine name = fromMaybe (Noco name) $ lookup name name2Compiler
        compilers = map fst name2Compiler
          
compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: cabal v2-run ifl -- [OPTION...] <program-file>"

---------------------------------------------------------------
-- MAIN
---------------------------------------------------------------

run :: Options -> [String] -> IO ()
run opts (file:_) = do
  hPutStrLn stderr $ "Program Source: " ++ file
  executer (optEngine opts) =<< readFile file

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
    else run opts rest
