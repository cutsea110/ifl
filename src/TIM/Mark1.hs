{-# LANGUAGE FlexibleInstances #-}
module TIM.Mark1
  ( parse
  , compile
  , eval
  , showResults
  , runProg
  ) where

import Heap
import Language
import Utils

runProg :: Bool -> String -> String
runProg verbose = showR . eval . compile . parse
  where showR | verbose   = showResults
              | otherwise = showSimpleResults

data Instruction = Take Int
                 | Enter TimAMode
                 | Push TimAMode
                 deriving (Eq, Show)

data TimAMode = Arg Int
              | Label Name
              | Code [Instruction]
              | IntConst Int
              deriving (Eq, Show)

data TimState = TimState { instructions :: [Instruction]
                         , frame        :: FramePtr
                         , stack        :: TimStack
                         , valstack     :: TimValueStack
                         , dump         :: TimDump
                         , heap         :: TimHeap
                         , codes        :: CodeStore
                         , stats        :: TimStats
                         }
              deriving (Eq, Show)

data FramePtr = FrameAddr Addr      -- The address of a frame
              | FrameInt Int        -- An integer value
              | FrameNull           -- Uninitialized
              deriving (Eq, Show)

type TimStack = [Closure]
type Closure = ([Instruction], FramePtr)

data TimValueStack = DummyTimValueStack
                   deriving (Eq, Show)
data TimDump = DummyTimDump
             deriving (Eq, Show)
type TimHeap = Heap Frame
instance {-# Overlapping #-} Show (Heap Frame) where
  show (allocs, size, _, cts) = show (allocs, size, cts)

type Frame = [Closure]

fAlloc :: TimHeap -> Frame -> (TimHeap, FramePtr)
fAlloc heap xs = (heap', FrameAddr addr)
  where
    (heap', addr) = hAlloc heap xs

fGet :: TimHeap -> FramePtr -> Int -> Closure
fGet heap (FrameAddr addr) n = f !! (n-1)
  where
    f = hLookup heap addr
fGet _ _ _ = error "fGet: not implemented"

fUpdate :: TimHeap -> FramePtr -> Int -> Closure -> TimHeap
fUpdate heap (FrameAddr addr) n closure
  = hUpdate heap addr new_frame
  where
    frame = hLookup heap addr
    new_frame = take (n-1) frame ++ [closure] ++ drop n frame
fUpdate _ _ _ _ = error "fUpdate: not implemented"

fList :: Frame -> [Closure]
fList f = f

type CodeStore = Assoc Name [Instruction]

codeLookup :: CodeStore -> Name -> [Instruction]
codeLookup cstore l
  = aLookup cstore l $ error $ "Attempt to jump to unknown label " ++ show l

type TimStats = Int  -- The number of steps

statInitial :: TimStats
statInitial = 0

statIncSteps :: TimStats -> TimStats
statIncSteps s = s + 1

statGetSteps :: TimStats -> Int
statGetSteps s = s

compile :: CoreProgram -> TimState
compile program
  = TimState { instructions = [Enter $ Label "main"]
             , frame        = FrameNull
             , stack        = initialArgStack
             , valstack     = initialValueStack
             , dump         = initialDump
             , heap         = hInitial
             , codes        = compiled_code
             , stats        = statInitial
             }
  where
    sc_defs = preludeDefs ++ program
    compiled_sc_defs = map (compileSc initial_env) sc_defs
    compiled_code = compiled_sc_defs ++ compiledPrimitives
    initial_env = [(name, Label name) | (name, _, _) <- sc_defs]
                  ++ [(name, Label name) | (name, _) <- compiledPrimitives]

initialArgStack :: TimStack
initialArgStack = []

initialValueStack :: TimValueStack
initialValueStack = DummyTimValueStack

initialDump :: TimDump
initialDump = DummyTimDump

compiledPrimitives :: [(Name, [Instruction])]
compiledPrimitives = []

type TimCompilerEnv = [(Name, TimAMode)]

compileSc :: TimCompilerEnv -> CoreScDefn -> (Name, [Instruction])
compileSc env (name, args, body)
  = (name, Take (length args) : instructions)
  where
    instructions = compileR body new_env
    new_env = zip args (map Arg [1..]) ++ env

compileR :: CoreExpr -> TimCompilerEnv -> [Instruction]
compileR (EAp e1 e2) env = Push (compileA e2 env) : compileR e1 env
compileR (EVar v) env = [Enter (compileA (EVar v) env)]
compileR (ENum n) env = [Enter (compileA (ENum n) env)]
compileR e        env = error $ "compileR: can't do this yet"

compileA :: CoreExpr -> TimCompilerEnv -> TimAMode
compileA (EVar v) env = aLookup env v $ error $ "Unknown variable " ++ v
compileA (ENum n) env = IntConst n
compileA e        env = Code $ compileR e env

eval :: TimState -> [TimState]
eval state = state : rest_states
  where rest_states | timFinal state = []
                    | otherwise      = eval next_state
        next_state = doAdmin $ step state

doAdmin :: TimState -> TimState
doAdmin state = applyToStats statIncSteps state

timFinal :: TimState -> Bool
timFinal state = null $ instructions state

applyToStats :: (TimStats -> TimStats) -> TimState -> TimState
applyToStats stats_fun state
  = state { stats = stats_fun $ stats state }

step :: TimState -> TimState
step = undefined

showResults :: [TimState] -> String
showResults = undefined

showSimpleResults :: [TimState] -> String
showSimpleResults = undefined

showFullResults :: [TimState] -> String
showFullResults = undefined

fullRun :: String -> String
fullRun = showFullResults . eval . compile . parse
