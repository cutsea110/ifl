module Main where

import Control.Monad (forM_, unless, when)
import Data.Char (toLower)
import Data.List (intercalate, foldl')
import Data.Maybe (fromMaybe)
import System.Console.GetOpt (OptDescr(..), ArgDescr(NoArg, ReqArg), ArgOrder(Permute)
                             , getOpt, usageInfo
                             )
import System.Environment (getArgs)
import System.IO (getContents, hPutStr, hPutStrLn
                 , stdout, stderr
                 , hSetBuffering, BufferMode(NoBuffering)
                 )

import qualified Template.Mark1      as Mark1 (runProg)
import qualified Template.Mark2      as Mark2 (runProg)
import qualified Template.Mark3      as Mark3 (runProg)
import qualified Template.Mark4      as Mark4 (runProg)
import qualified Template.Mark5      as Mark5      (runProg, Config(..))
import qualified Template.Mark5Alt   as Mark5Alt   (runProg, Config(..))
import qualified Template.Mark5GC    as Mark5GC    (runProg, Config(..))
import qualified Template.Mark5RevGC as Mark5RevGC (runProg, Config(..))
import qualified Template.Mark5Cp    as Mark5Cp    (runProg, Config(..))

import qualified Gmachine.Mark1 as GMark1 (runProg)
import qualified Gmachine.Mark2 as GMark2 (runProg)
import qualified Gmachine.Mark3 as GMark3 (runProg)
import qualified Gmachine.Mark4 as GMark4 (runProg)
import qualified Gmachine.Mark5 as GMark5 (runProg)
import qualified Gmachine.Mark6 as GMark6 (runProg, Config(..))
import qualified Gmachine.Mark7 as GMark7 (runProg, Config(..))

import qualified TIM.Mark1   as TIMark1   (runProg, Config(..))
import qualified TIM.Mark1Cp as TIMark1Cp (runProg, Config(..))
import qualified TIM.Mark2   as TIMark2   (runProg, Config(..))
import qualified TIM.Mark3   as TIMark3   (runProg, Config(..))

---------------------------------------------------------------
-- COMPILER
---------------------------------------------------------------
type Executer = String -> IO ()

executer :: Options -> Executer
executer opts = putStr . run
  where verbose = optVerbose opts
        compiler = optCompiler opts
        threshold = optThreshold opts
        convertList = optConvertList opts
        run = case compiler of
          Mark1         -> Mark1.runProg
          Mark2         -> Mark2.runProg
          Mark3         -> Mark3.runProg
          Mark4         -> Mark4.runProg
          Mark5         -> Mark5.runProg      $ Mark5.Config convertList
          Mark5Alt      -> Mark5Alt.runProg   $ Mark5Alt.Config convertList
          Mark5GC       -> Mark5GC.runProg    $ Mark5GC.Config threshold convertList
          Mark5RevGC    -> Mark5RevGC.runProg $ Mark5RevGC.Config threshold convertList
          Mark5Cp       -> Mark5Cp.runProg    $ Mark5Cp.Config verbose threshold convertList
          GMark1        -> GMark1.runProg
          GMark2        -> GMark2.runProg
          GMark3        -> GMark3.runProg
          GMark4        -> GMark4.runProg
          GMark5        -> GMark5.runProg
          GMark6        -> GMark6.runProg    $ GMark6.Config verbose
          GMark7        -> GMark7.runProg    $ GMark7.Config verbose
          TIMark1       -> TIMark1.runProg   $ TIMark1.Config verbose
          TIMark1Cp     -> TIMark1Cp.runProg $ TIMark1Cp.Config verbose threshold
          TIMark2       -> TIMark2.runProg   $ TIMark2.Config verbose threshold
          TIMark3       -> TIMark3.runProg   $ TIMark3.Config verbose threshold
          (Noco name)   -> const $ "Error: Unknown compiler = " ++ name ++ "\n" ++ helpMessage

---------------------------------------------------------------
-- COMMAND LINE OPTIONS
---------------------------------------------------------------

data Compiler
  = Noco !String
  | Mark1 | Mark2 | Mark3 | Mark4
  | Mark5 | Mark5Alt | Mark5GC | Mark5RevGC | Mark5Cp
  | GMark1 | GMark2 | GMark3 | GMark4 | GMark5 | GMark6 | GMark7
  | TIMark1 | TIMark1Cp | TIMark2 | TIMark3
  deriving (Show, Eq)

validCompiler :: Compiler -> Bool
validCompiler (Noco _) = False
validCompiler _        = True

data Options = Options
  { optVerbose     :: !Bool
  , optThreshold   :: !Int
  , optShowVersion :: !Bool
  , optCompiler    :: !Compiler
  , optConvertList :: !Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { optVerbose     = False
  , optThreshold   = 100
  , optShowVersion = False
  , optCompiler    = TIMark3
  , optConvertList = False
  }

name2Compiler :: [(String, Compiler)]
name2Compiler
  = map (\c -> (map toLower (show c), c))
    [ Mark1, Mark2, Mark3, Mark4
    , Mark5, Mark5Alt, Mark5GC, Mark5RevGC, Mark5Cp
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
            -- NOTE: this option is only for the part of Template Instantiation Machines.
          , Option ['l']      ["convert-to-list-based"]   (NoArg (\opts -> opts {optConvertList = True}))
            "convert to list based program"
          , Option ['V', '?'] ["version"]   (NoArg (\opts -> opts {optShowVersion = True}))
            "show version"
          ]
  where decide :: String -> Compiler
        decide name = fromMaybe (Noco name) $ lookup name name2Compiler
          
compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o, n, []  ) -> return (foldl' (flip id) defaultOptions o, n)
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
  warnMessage opts
  when (optVerbose opts) $ do
    preprint
  prog <- readFile fp
  executer opts prog
  where
    preprint :: IO ()
    preprint = do
      hPutStrLn stderr $ "       Program Source: " ++ fp
      hPutStrLn stderr $ "     Choosed Compiler: " ++ show (optCompiler opts)
      hPutStrLn stderr $ "              Verbose: " ++ show (optVerbose opts)
      hPutStrLn stderr $ "         GC Threshold: " ++ show (optThreshold opts)
      hPutStrLn stderr $ "Convert to List Based: " ++ show (optConvertList opts)
      hPutStrLn stderr $
        "The compilers that can be specified are as follows: " ++ intercalate "," compilerNames ++ "."

checkOption :: Options -> [String]
checkOption opts = compilerSupported ++ convToListSupported ++ gcThresholdSupported
  where
    compiler = optCompiler opts
    compilerSupported
      | validCompiler compiler = []
      | otherwise = ["The compiler is not supported."]
    convToListSupported
      | not (optConvertList opts) ||
        compiler `elem` [Mark5, Mark5Alt, Mark5GC, Mark5RevGC, Mark5Cp] = []
      | otherwise = ["The compiler does not support the option of converting to list based program."]
    gcThresholdSupported
      | optThreshold opts == optThreshold defaultOptions ||
        compiler `elem` [Mark5GC, Mark5RevGC, Mark5Cp, TIMark1Cp, TIMark2, TIMark3] = []
      | otherwise = ["The compiler does not support the option of GC threshold."]

warnMessage :: Options -> IO ()
warnMessage opts = do
  unless (null msgs) $ do
    mapM_ (hPutStrLn stderr . ("[WARN] " ++)) $ checkOption opts
    hPutStrLn stderr "------"
  where msgs = checkOption opts


main :: IO ()
main = do
  args <- getArgs
  (opts, rest) <- compilerOpts args

  unless (optVerbose opts) $ do
    hSetBuffering stdout NoBuffering

  case rest of
    []    -> hPutStr stderr helpMessage
    [src] -> run opts src
    (_:_) -> error "Too many arguments"
