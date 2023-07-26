module TIM.Mark1 where

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

data TimAMode = Arg Int
              | Label Name
              | Code [Instruction]
              | IntConst Int

data TimState = TimState { instructions :: [Instruction]
                         , frame        :: FramePtr
                         , stack        :: TimStack
                         , valstack     :: TimValueStack
                         , dump         :: TimDump
                         , heap         :: TimHeap
                         , codes        :: CodeStore
                         , stats        :: TimStats
                         }

data FramePtr = FrameAddr Addr
              | FrameInt Int
              | FrameNull

type TimStack = [Closure]
type Closure = ([Instruction], FramePtr)

data TimValueStack = DummyTimValueStack
data TimDump = DummyTimDump
type TimHeap = Heap Frame
type Frame = [Closure]
type CodeStore = Assoc Name [Instruction]
type TimStats = Int

compile :: CoreProgram -> TimState
compile = undefined

eval :: TimState -> [TimState]
eval = undefined

showResults :: [TimState] -> String
showResults = undefined

showSimpleResults :: [TimState] -> String
showSimpleResults = undefined

