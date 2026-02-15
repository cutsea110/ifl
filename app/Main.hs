module Main where

import Control.Monad (forM_, unless, when)
import Data.Char (toLower)
import Data.List (intercalate, foldl')
import Data.Maybe (fromMaybe)
import System.Console.GetOpt (OptDescr(..), ArgDescr(NoArg, ReqArg), ArgOrder(Permute), getOpt, usageInfo)
import System.Environment (getArgs)
import System.IO (getContents, hPutStr, hPutStrLn, stdout, stderr, hSetBuffering, BufferMode(NoBuffering))

import Language (parse, pprint, CoreProgram)

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
import qualified TIM.Mark4   as TIMark4   (runProg, Config(..))
import qualified TIM.Mark5   as TIMark5   (runProg, Config(..))
import qualified TIM.Mark6   as TIMark6   (runProg, Config(..))

import qualified Pgmachine.Mark1 as PgMark1   (runProg, Config(..))
import qualified Pgmachine.Mark2 as PgMark2   (runProg, Config(..))
import qualified Pgmachine.Mark3 as PgMark3   (runProg, Config(..))
import qualified Pgmachine.Mark4 as PgMark4   (runProg, Config(..))

import qualified Lambda.Mark1 as LMark1 (lambdaLift)
import qualified Lambda.Mark2 as LMark2 (lambdaLift)
import qualified Lambda.Mark3 as LMark3 (lambdaLiftJ)
import qualified Lambda.Mark4 as LMark4 (fullyLazyLift)

---------------------------------------------------------------
-- COMPILER
---------------------------------------------------------------
type Executer = String -> IO ()

lambdaLifter :: Options -> CoreProgram -> CoreProgram
lambdaLifter opts
  = case optLifter opts of
      LMark1 -> LMark1.lambdaLift
      LMark2 -> LMark2.lambdaLift
      LMark3 -> LMark3.lambdaLiftJ
      LMark4 -> LMark4.fullyLazyLift
      NoLift -> id

executer :: Options -> Executer
executer opts = putStr . run
  where verbose = optVerbose opts || werbose
        werbose = optWerbose opts
        compiler = optCompiler opts
        lifter = lambdaLifter opts
        threshold = optThreshold opts
        machineSize = optMachineSize opts
        convertList = optConvertList opts
        profile = optProfile opts
        run = case compiler of
          Mark1       -> Mark1.runProg
          Mark2       -> Mark2.runProg
          Mark3       -> Mark3.runProg
          Mark4       -> Mark4.runProg
          Mark5       -> Mark5.runProg      $ Mark5.Config convertList lifter
          Mark5Alt    -> Mark5Alt.runProg   $ Mark5Alt.Config convertList lifter
          Mark5GC     -> Mark5GC.runProg    $ Mark5GC.Config threshold convertList lifter
          Mark5RevGC  -> Mark5RevGC.runProg $ Mark5RevGC.Config threshold convertList lifter
          Mark5Cp     -> Mark5Cp.runProg    $ Mark5Cp.Config verbose threshold convertList lifter
          GMark1      -> GMark1.runProg
          GMark2      -> GMark2.runProg
          GMark3      -> GMark3.runProg
          GMark4      -> GMark4.runProg
          GMark5      -> GMark5.runProg
          GMark6      -> GMark6.runProg    $ GMark6.Config verbose lifter
          GMark7      -> GMark7.runProg    $ GMark7.Config verbose lifter
          TIMark1     -> TIMark1.runProg   $ TIMark1.Config verbose lifter
          TIMark1Cp   -> TIMark1Cp.runProg $ TIMark1Cp.Config verbose threshold lifter
          TIMark2     -> TIMark2.runProg   $ TIMark2.Config verbose threshold lifter
          TIMark3     -> TIMark3.runProg   $ TIMark3.Config verbose threshold lifter
          TIMark4     -> TIMark4.runProg   $ TIMark4.Config verbose threshold lifter
          TIMark5     -> TIMark5.runProg   $ TIMark5.Config verbose threshold convertList lifter
          TIMark6     -> TIMark6.runProg   $ TIMark6.Config verbose threshold convertList profile lifter
          PgMark1     -> PgMark1.runProg   $ PgMark1.Config verbose werbose lifter
          PgMark2     -> PgMark2.runProg   $ PgMark2.Config verbose werbose lifter
          PgMark3     -> PgMark3.runProg   $ PgMark3.Config verbose werbose machineSize lifter
          PgMark4     -> PgMark4.runProg   $ PgMark4.Config verbose werbose machineSize lifter
          (Noco name) -> const $ "Error: Unknown compiler = " ++ name ++ "\n" ++ helpMessage

---------------------------------------------------------------
-- COMMAND LINE OPTIONS
---------------------------------------------------------------

data Compiler
  = Noco !String
  | Mark1 | Mark2 | Mark3 | Mark4
  | Mark5 | Mark5Alt | Mark5GC | Mark5RevGC | Mark5Cp
  | GMark1 | GMark2 | GMark3 | GMark4 | GMark5 | GMark6 | GMark7
  | TIMark1 | TIMark1Cp | TIMark2 | TIMark3 | TIMark4 | TIMark5 | TIMark6
  | PgMark1 | PgMark2 | PgMark3 | PgMark4
  deriving (Show, Eq)

validCompiler :: Compiler -> Bool
validCompiler (Noco _) = False
validCompiler _        = True

data Options = Options
  { optVerbose     :: !Bool
  , optWerbose     :: !Bool -- more verbose output, showHeap!
  , optThreshold   :: !Int
  , optMachineSize :: !Int -- machine size for Parallel G-machine
  , optShowVersion :: !Bool
  , optCompiler    :: !Compiler
  , optLifter      :: !LambdaLifter
  , optConvertList :: !Bool
  , optProfile     :: !Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { optVerbose     = False
  , optWerbose     = False
  , optThreshold   = 300
  , optMachineSize = 4
  , optShowVersion = False
  , optCompiler    = PgMark4
  , optLifter      = LMark4
  , optConvertList = False
  , optProfile     = False
  }

name2Compiler :: [(String, Compiler)]
name2Compiler
  = map (\c -> (map toLower (show c), c))
    [ Mark1, Mark2, Mark3, Mark4
    , Mark5, Mark5Alt, Mark5GC, Mark5RevGC, Mark5Cp
    , GMark1, GMark2, GMark3, GMark4, GMark5, GMark6, GMark7
    , TIMark1, TIMark1Cp, TIMark2, TIMark3, TIMark4, TIMark5, TIMark6
    , PgMark1, PgMark2, PgMark3, PgMark4
    ]

compilerNames :: [String]
compilerNames = map fst name2Compiler

data LambdaLifter = LMark1
                  | LMark2
                  | LMark3
                  | LMark4
                  | NoLift
                  deriving (Show, Eq)

name2Lifter :: [(String, LambdaLifter)]
name2Lifter
  = map (\c -> (map toLower (show c), c))
    [ LMark1, LMark2, LMark3, LMark4 ]

options :: [OptDescr (Options -> Options)]
options = [ Option ['c'] ["compiler"] (ReqArg (\e opts -> opts {optCompiler = compiler e}) "Compiler")
            ("compiler name (" ++ intercalate " | " compilerNames ++ ")")
          , Option ['l'] ["lifter"] (ReqArg (\e opts -> opts {optLifter = lifter e}) "Lifter")
            ("lambda lifter name (" ++ intercalate " | " (map fst name2Lifter) ++ ")")
          , Option ['v'] ["verbose"] (NoArg (\opts -> opts {optVerbose = True}))
            "step output on stderr"
          , Option ['w'] ["pretty verbose"] (NoArg (\opts -> opts {optWerbose = True}))
            "step output with showing heap on stderr"
          , Option ['t'] ["threshold"] (ReqArg (\n opts -> opts {optThreshold = read n}) "Threshold")
            "threshold for Garbage Collection"
          , Option ['m'] ["machines"] (ReqArg (\n opts -> opts {optMachineSize = read n}) "MachineSize")
            "Machine size for Parallel G-machine (default: 4)"
            -- NOTE: this option is only for the part of Template Instantiation Machines.
          , Option ['L'] ["convert-to-list-based"] (NoArg (\opts -> opts {optConvertList = True}))
            "convert to list based program"
            -- NOTE: this option is my original option. It's not in textbook.
          , Option ['p'] ["profile"] (NoArg (\opts -> opts {optProfile = True}))
            "profile output"
          , Option ['V', '?'] ["version"]   (NoArg (\opts -> opts {optShowVersion = True}))
            "show version"
          ]
  where compiler :: String -> Compiler
        compiler name = fromMaybe (Noco name) $ lookup name name2Compiler
        lifter :: String -> LambdaLifter
        lifter name = fromMaybe NoLift $ lookup name name2Lifter

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

warnMessage :: Options -> IO ()
warnMessage opts = do
  unless (null msgs) $ do
    mapM_ (hPutStrLn stderr . ("[WARN] " ++)) msgs
    hPutStrLn stderr "------"
  where msgs = checkOption opts

settingInfos :: Options -> FilePath -> String
settingInfos opts fp =
  unlines [ "       Program Source: " ++ fp
          , "     Choosed Compiler: " ++ show (optCompiler opts)
          , "Choosed Lambda Lifter: " ++ show (optLifter opts)
          , "              Verbose: " ++ show (optVerbose opts)
          , "       Pretty verbose: " ++ show (optWerbose opts)
          , "         GC Threshold: " ++ show (optThreshold opts)
          , "         Machine Size: " ++ show (optMachineSize opts)
          , "Convert to List Based: " ++ show (optConvertList opts)
          , "The compilers that can be specified are as follows: " ++
            intercalate "," compilerNames ++ "."
          ]

checkOption :: Options -> [String]
checkOption opts = compilerSupported ++ convToListSupported ++ gcThresholdSupported
  where
    compiler = optCompiler opts
    compilerSupported
      | validCompiler compiler = []
      | otherwise = ["The compiler is not supported."]
    convToListSupported
      | not (optConvertList opts) ||
        compiler `elem` [Mark5, Mark5Alt, Mark5GC, Mark5RevGC, Mark5Cp, TIMark5, TIMark6] = []
      | otherwise = ["The compiler does not support the option of converting to list based program."]
    profileSupported
      | not (optProfile opts) ||
        compiler `elem` [TIMark6] = []
      | otherwise = ["The compiler does not support the option of profiling a program."]
    worvoseSupported
      | not (optWerbose opts) ||
        compiler `elem` [PgMark1, PgMark2] = []
      | otherwise = ["The compiler does not support the option of pretty verbose output."]
    gcThresholdSupported
      | optThreshold opts == optThreshold defaultOptions ||
        compiler `elem` [Mark5GC, Mark5RevGC, Mark5Cp, TIMark1Cp, TIMark2, TIMark3, TIMark4, TIMark5, TIMark6] = []
      | otherwise = ["The compiler does not support the option of GC threshold."]
    machineSizeSupported
      | optMachineSize opts == optMachineSize defaultOptions ||
        compiler `elem` [PgMark3] = []
      | otherwise = ["The compiler does not support the option of machine size."]

run :: Options -> FilePath -> IO ()
run opts fp = do
  warnMessage opts

  when (optVerbose opts || optWerbose opts) $ do
    hPutStrLn stderr (settingInfos opts fp)

  prog <- readFile fp

  when (optVerbose opts || optWerbose opts) $ do
    hPutStrLn stderr "Compiled Code"
    hPutStrLn stderr ""
    hPutStrLn stderr "-------------------------------"
    hPutStrLn stderr (pprint (lambdaLifter opts (parse prog)))
    hPutStrLn stderr "-------------------------------"
    hPutStrLn stderr ""

  executer opts prog

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
