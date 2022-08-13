module Gmachine.Mark1 where

import Heap
import Iseq
import Language
import Parser
import Stack
import Utils

runProg :: String -> String
runProg = showResults . eval . compile . parse

showResults = undefined
eval = undefined
compile = undefined

data GmState = GmState { code    :: GmCode
                       , stack   :: GmStack
                       , heap    :: GmHeap
                       , globals :: GmGlobals
                       , stats   :: GmStats
                       }

type GmCode = [Instruction]

getCode :: GmState -> GmCode
getCode state = code state

putCode :: GmCode -> GmState -> GmState
putCode i' state = state { code = i' }

data Instruction
  = Unwind
  | Pushglobal Name
  | Pushint Int
  | Mkap
  | Slide Int
  deriving (Eq)

type GmStack = Stack Addr

getStack :: GmState -> GmStack
getStack state = stack state

putStack :: GmStack -> GmState -> GmState
putStack stack' state = state { stack = stack' }

type GmHeap = Heap Node

getHeap :: GmState -> GmHeap
getHeap state = heap state

putHeap :: GmHeap -> GmState -> GmState
putHeap heap' state = state { heap = heap' }

data Node
  = NNum Int           -- Numbers
  | NAp Addr Addr      -- Applications
  | NGlobal Int GmCode -- Globals


type GmGlobals = Assoc Name Addr

getGlobals :: GmState -> GmGlobals
getGlobals state = globals state

putGlobals :: GmGlobals -> GmState -> GmState
putGlobals globals' state = state { globals = globals' }

type GmStats = Int

statInitial :: GmStats
statInitial = 0

statIncSteps :: GmStats -> GmStats
statIncSteps s = s + 1

statGetSteps :: GmStats -> Int
statGetSteps s = s

getStats :: GmState -> GmStats
getStats state = stats state

putStats :: GmStats -> GmState -> GmState
putStats stats' state = state { stats = stats' }
