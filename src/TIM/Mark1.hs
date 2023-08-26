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
compile = undefined

eval :: TimState -> [TimState]
eval = undefined

showResults :: [TimState] -> String
showResults = undefined

showSimpleResults :: [TimState] -> String
showSimpleResults = undefined

showFullResults :: [TimState] -> String
showFullResults = undefined

fullRun :: String -> String
fullRun = showFullResults . eval . compile . parse
