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
import qualified Template.Mark5Alt   as Mark5Alt (runProg, runProgWithConv)
import qualified Template.Mark5GC    as Mark5GC (runProg, runProgWithConv)
import qualified Template.Mark5RevGC as Mark5RevGC (runProg, runProgWithConv)
import qualified Template.Mark5Cp    as Mark5Cp (runProg, runProgWithConv)

import qualified Gmachine.Mark1 as GMark1 (runProg)
import qualified Gmachine.Mark2 as GMark2 (runProg)
import qualified Gmachine.Mark3 as GMark3 (runProg)
import qualified Gmachine.Mark4 as GMark4 (runProg)
import qualified Gmachine.Mark5 as GMark5 (runProg)
import qualified Gmachine.Mark6 as GMark6 (runProg)
import qualified Gmachine.Mark7 as GMark7 (runProg, Config(..))

import qualified TIM.Mark1   as TIMark1 (runProg, Config(..))
import qualified TIM.Mark1Cp as TIMark1Cp (runProg, Config(..))
import qualified TIM.Mark2   as TIMark2 (runProg, Config(..))
import qualified TIM.Mark3   as TIMark3 (runProg, Config(..))

---------------------------------------------------------------
-- COMPILER
---------------------------------------------------------------
type Executer = String -> IO ()

executer :: Options -> Executer
executer opts = putStr . run
  where verbose = optVerbose opts
        compiler = optCompiler opts
        threshold = optThreshold opts
        run = case compiler of
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
          GMark7        -> GMark7.runProg $ GMark7.Config verbose
          TIMark1       -> TIMark1.runProg $ TIMark1.Config verbose
          TIMark1Cp     -> TIMark1Cp.runProg $ TIMark1Cp.Config verbose threshold
          TIMark2       -> TIMark2.runProg $ TIMark2.Config verbose threshold
          TIMark3       -> TIMark3.runProg $ TIMark3.Config verbose threshold
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
  | TIMark1 | TIMark1Cp | TIMark2 | TIMark3
  deriving Show

data Options = Options
  { optVerbose     :: Bool
  , optThreshold   :: Int
  , optShowVersion :: Bool
  , optCompiler    :: Compiler
  }

defaultOptions :: Options
defaultOptions = Options
  { optVerbose     = False
  , optThreshold   = 100
  , optShowVersion = False
  , optCompiler    = TIMark3
  }

name2Compiler :: [(String, Compiler)]
name2Compiler
  = map (\c -> (map toLower (show c), c))
    [ Mark1, Mark2, Mark3, Mark4
    , Mark5, Mark5cnv, Mark5Alt, Mark5Altcnv, Mark5GC, Mark5GCcnv
    , Mark5RevGC, Mark5RevGCcnv, Mark5Cp
    , GMark1, GMark2, GMark3, GMark4, GMark5, GMark6, GMark7
    , TIMark1, TIMark1Cp, TIMark2, TIMark3
    ]

compilerNames :: [String]
compilerNames = map fst name2Compiler

options :: [OptDescr (Options -> Options)]
options = [ Option ['c']      ["compiler"]  (ReqArg (\e opts -> opts {optCompiler = decide e}) "Compiler")
            ("compiler name (" ++ intercalate " | " compilerNames ++ ")")
          , Option ['v']      ["verbose"]   (NoArg (\opts -> opts {optVerbose = True}))
            "step output on stderr"
          , Option ['t']      ["threshold"] (ReqArg (\n opts -> opts {optThreshold = read n}) "Threshold")
            "threshold for Garbage Collection"
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
  when (optVerbose opts) $ do
    preprint
  prog <- readFile fp
  executer opts prog
  where
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
    []    -> hPutStr stderr helpMessage
    [src] -> run opts src
    _     -> error "Too many arguments"
