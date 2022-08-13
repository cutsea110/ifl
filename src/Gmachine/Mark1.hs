module Gmachine.Mark1 where

import Heap
import Iseq
import Language
import Parser
import qualified Stack as S
import Utils

runProg :: String -> String
runProg = showResults . eval . compile . parse

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
  | Push Int -- push offset
  | Slide Int
  deriving (Eq)

type GmStack = S.Stack Addr

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



eval :: GmState -> [GmState]
eval state = state : restStates
  where
    restStates | gmFinal state = []
               | otherwise     = eval nextState
    nextState  = doAdmin (step state)


doAdmin :: GmState -> GmState
doAdmin s = putStats (statIncSteps (getStats s)) s

gmFinal :: GmState -> Bool
gmFinal s = null $ getCode s

step :: GmState -> GmState
step state = dispatch i (putCode is state)
  where i:is = getCode state

dispatch :: Instruction -> GmState -> GmState
dispatch (Pushglobal f) = pushglobal f
dispatch (Pushint n)    = pushint n
dispatch Mkap           = mkap
dispatch (Push n)       = push n
dispatch (Slide n)      = slide n
dispatch Unwind         = unwind

pushglobal :: Name -> GmState -> GmState
pushglobal f state = putStack (S.push a $ getStack state) state
  where a = aLookup (getGlobals state) f (error $ "Undeclared global " ++ f)

pushint :: Int -> GmState -> GmState
pushint n state = putHeap heap' (putStack (S.push a $ getStack state) state)
  where (heap', a) = hAlloc (getHeap state) (NNum n)

mkap :: GmState -> GmState
mkap state = putHeap heap' (putStack (S.push a s2) state)
  where (heap', a)  = hAlloc (getHeap state) (NAp a1 a2)
        (a1, s1) = S.pop $ getStack state
        (a2, s2) = S.pop s1

push :: Int -> GmState -> GmState
push n state = putStack (S.push a s) state
  where s = getStack state
        a = getArg (hLookup (getHeap state) (S.getStack s !! (n+1)))

getArg :: Node -> Addr
getArg (NAp _ a2) = a2
getArg _          = error "not application Node"

slide :: Int -> GmState -> GmState
slide n state = putStack (S.push a $ S.discard n s) state
  where (a, s) = S.pop $ getStack state

unwind :: GmState -> GmState
unwind state = newState (hLookup heap a)
  where s = getStack state
        (a, s1) = S.pop s
        heap   = getHeap state
        newState (NNum n) = state
        newState (NAp a1 a2) = putCode [Unwind] (putStack (S.push a1 s) state)
        newState (NGlobal n c)
          | S.getDepth s1 < n = error "Unwinding with too few arguments"
          | otherwise         = putCode c state

showResults = undefined
compile = undefined

