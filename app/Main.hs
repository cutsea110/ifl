module Main where

import Control.Monad (forM_, unless, when)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import System.Console.GetOpt (OptDescr(..), ArgDescr(NoArg, ReqArg), ArgOrder(Permute)
                             , getOpt, usageInfo
                             )
import System.Environment (getArgs)
import System.IO (getContents, hPutStr, hPutStrLn
                 , stdout, stderr
                 , hSetBuffering, BufferMode(NoBuffering)
                 )

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
import qualified Gmachine.Mark7 as GMark7 (runProg)

import qualified TIM.Mark1 as TIMark1 (runProg)

---------------------------------------------------------------
-- COMPILER
---------------------------------------------------------------
type Executer = String -> IO ()

executer :: Compiler -> Bool -> Executer
executer e verbose = hPutStr stdout . run
  where run = case e of
          Mark1         -> Mark1.runProg
          Mark2         -> Mark2.runProg
          Mark3         -> Mark3.runProg
          Mark4         -> Mark4.runProg
          Mark5         -> Mark5.runProg
          Mark5cnv      -> Mark5.runProgWithConv
          Mark5Alt      -> Mark5Alt.runProg
          Mark5Altcnv   -> Mark5Alt.runProgWithConv
          Mark5GC       -> Mark5GC.runProg
          Mark5GCcnv    -> Mark5GC.runProgWithConv
          Mark5RevGC    -> Mark5RevGC.runProg
          Mark5RevGCcnv -> Mark5RevGC.runProgWithConv
          Mark5Cp       -> Mark5Cp.runProg
          Mark5Cpcnv    -> Mark5Cp.runProgWithConv
          GMark1        -> GMark1.runProg
          GMark2        -> GMark2.runProg
          GMark3        -> GMark3.runProg
          GMark4        -> GMark4.runProg
          GMark5        -> GMark5.runProg
          GMark6        -> GMark6.runProg verbose
          GMark7        -> GMark7.runProg verbose
          TIMark1       -> TIMark1.runProg verbose
          (Noco name)   -> const $ "Error: Unknown compiler = " ++ name ++ "\n" ++ helpMessage

---------------------------------------------------------------
-- COMMAND LINE OPTIONS
---------------------------------------------------------------

data Compiler
  = Noco String
  | Mark1 | Mark2 | Mark3 | Mark4
  | Mark5 | Mark5cnv | Mark5Alt | Mark5Altcnv | Mark5GC | Mark5GCcnv
  | Mark5RevGC | Mark5RevGCcnv | Mark5Cp | Mark5Cpcnv
  | GMark1 | GMark2 | GMark3 | GMark4 | GMark5 | GMark6 | GMark7
  | TIMark1
  deriving Show

data Options = Options
  { optVerbose     :: Bool
  , optShowVersion :: Bool
  , optCompiler    :: Compiler
  }

defaultOptions :: Options
defaultOptions = Options
  { optVerbose     = False
  , optShowVersion = False
  , optCompiler    = TIMark1
  }

name2Compiler :: [(String, Compiler)]
name2Compiler
  = map (\c -> (map toLower (show c), c))
    [ Mark1, Mark2, Mark3, Mark4
    , Mark5, Mark5cnv, Mark5Alt, Mark5Altcnv, Mark5GC, Mark5GCcnv
    , Mark5RevGC, Mark5RevGCcnv, Mark5Cp
    , GMark1, GMark2, GMark3, GMark4, GMark5, GMark6, GMark7
    , TIMark1
    ]

compilerNames :: [String]
compilerNames = map fst name2Compiler

options :: [OptDescr (Options -> Options)]
options = [ Option ['c']      ["compiler"]  (ReqArg (\e opts -> opts {optCompiler = decide e}) "Compiler")
            ("compiler name (" ++ intercalate " | " compilerNames ++ ")")
          , Option ['v']      ["verbose"]   (NoArg (\opts -> opts {optVerbose = True}))
            "step output on stderr"
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
  where header = "Usage: cabal run ifl -- [OPTION...] <program-file>"

---------------------------------------------------------------
-- MAIN
---------------------------------------------------------------

-- This banner generated by using 'figlet -f slant IFL'
helpMessage :: String
helpMessage =
  unlines [ "    ____________"
          , "   /  _/ ____/ /"
          , "   / // /_  / /"
          , " _/ // __/ / /___"
          , "/___/_/   /_____/ Implimenting Functional Languages"
          , ""
          , "> cabal run ifl -- [OPTION...] <program-file>"
          , usageInfo "OPTION" options
          ]

run :: Options -> FilePath -> IO ()
run opts fp = do
  when verbose $ do
    preprint
  prog <- readFile fp
  executer compiler verbose prog
  where
    compiler = optCompiler opts
    verbose = optVerbose opts
    preprint :: IO ()
    preprint = do
      hPutStrLn stderr $ "Program Source: " ++ fp
      hPutStrLn stderr $
        "The compilers that can be specified are as follows: " ++ intercalate "," compilerNames ++ "."

main :: IO ()
main = do
  args <- getArgs
  (opts, rest) <- compilerOpts args

  unless (optVerbose opts) $ do
    hSetBuffering stdout NoBuffering

  case rest of
    [] -> hPutStr stderr helpMessage
    (src:_) -> run opts src
