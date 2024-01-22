{-# LANGUAGE FlexibleInstances #-}
module TIM.Mark2
  ( parse
  , compile
  , eval
  , showResults
  , runProg
  ) where

import Control.Arrow ((&&&), second)
import Data.List (mapAccumL, nub, sort)
import Debug.Trace (trace)

import Heap
import Iseq
import Language
import Utils

-- for GC
threshold :: Int
threshold = 100

runProg :: Bool -> String -> String
runProg verbose = showR . eval . compile . parse
  where showR | verbose   = showResults
              | otherwise = showSimpleResult

data Instruction = Take Int
                 | Enter TimAMode
                 | Push TimAMode
                 | PushV ValueAMode
                 | Return
                 | Op Op
                 | Cond [Instruction] [Instruction]
                 deriving (Eq, Show)

type UsedSlots = [Int]
data CompiledCode = Compiled { slotsOf  :: UsedSlots
                             , instrsOf :: [Instruction]
                             }
                  deriving (Eq, Show)

data Op = Add | Sub | Mul | Div | Neg
        | Eq | Ne | Lt | Le | Gt | Ge
        deriving (Eq, Show)

data ValueAMode = FramePtr
                | IntVConst Int
                deriving (Eq, Show)


data TimAMode = Arg Int
              | Label Name
              | Code CompiledCode
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

getInstructions :: TimState -> [Instruction]
getInstructions = instructions
putInstructions :: [Instruction] -> TimState -> TimState
putInstructions instrs state = state { instructions = instrs }
getFrame :: TimState -> FramePtr
getFrame = frame
putFrame :: FramePtr -> TimState -> TimState
putFrame fr state = state { frame = fr }
getStack :: TimState -> TimStack
getStack = stack
putStack :: TimStack -> TimState -> TimState
putStack stk state = state { stack = stk, stats = sts }
  where
    dwh = length stk
    sts = statUpdateMaxStackDepth dwh (getStats state)
getVStack :: TimState -> TimValueStack
getVStack = valstack
putVStack :: TimValueStack -> TimState -> TimState
putVStack vstk state = state { valstack = vstk }
getDump :: TimState -> TimDump
getDump = dump
putDump :: TimDump -> TimState -> TimState
putDump dmp state = state { dump = dmp }
getHeap :: TimState -> TimHeap
getHeap = heap
putHeap :: TimHeap -> TimState -> TimState
putHeap hp state = state { heap = hp }
getCodes :: TimState -> CodeStore
getCodes = codes
putCodes :: CodeStore -> TimState -> TimState
putCodes cds state = state { codes = cds }
getStats :: TimState -> TimStats
getStats = stats
putStats :: TimStats -> TimState -> TimState
putStats sts state = state { stats = sts }


data FramePtr = FrameAddr Addr      -- The address of a frame
              | FrameInt Int        -- An integer value
              | FrameNull           -- Uninitialized
              deriving (Eq, Show)

type TimStack = [Closure]
type Closure = ([Instruction], FramePtr)

type TimValueStack = [Int]
data TimDump = DummyTimDump
             deriving (Eq, Show)
type TimHeap = Heap Frame
instance {-# Overlapping #-} Show (Heap Frame) where
  -- NOTE: addr field is infinite list, so we shouldn't show it.
  show (allocs, size, _, cts) = show (allocs, size, cts)

data Frame = Frame [Closure]
           | Forward Addr
           deriving (Eq, Show)

fAlloc :: TimHeap -> Frame -> (TimHeap, FramePtr)
fAlloc heap xs = (heap', FrameAddr addr)
  where
    (heap', addr) = hAlloc heap xs

fGet :: TimHeap -> FramePtr -> Int -> Closure
fGet heap (FrameAddr addr) n = case frm of
  Frame f      -> f !! (n-1)
  Forward addr -> error $ "fGet: Unexpected " ++ show frm
  where
    frm = hLookup heap addr
fGet _ _ _ = error "fGet: not implemented"

fUpdate :: TimHeap -> FramePtr -> Int -> Closure -> TimHeap
fUpdate heap (FrameAddr addr) n closure
  = hUpdate heap addr new_frame
  where
    frame = case frm of
      Frame f      -> f
      Forward addr -> error $ "fUpdate: Unexpected " ++ show frm
      where
        frm = hLookup heap addr
    new_frame = Frame $ take (n-1) frame ++ [closure] ++ drop n frame
fUpdate _ _ _ _ = error "fUpdate: not implemented"

fList :: Frame -> Either Addr [Closure]
fList (Frame f)   = Right f
fList (Forward a) = Left a

type CodeStore = Assoc Name CompiledCode

codeLookup :: CodeStore -> Name -> [Instruction]
codeLookup cstore l = instrsOf cs
  where cs = aLookup cstore l $ error $ "Attempt to jump to unknown label " ++ show l

data TimStats
  = TimStats { getSteps         :: Int  -- The number of steps
             , getExecTime      :: Int  -- The execution time
             , getHeapAllocated :: Int  -- The amount of heap allocated
             , getMaxStackDepth :: Int  -- The maximum stack depth
             , getGCInfo        :: [( Int  -- The number of steps
                                    , Size -- The size of the heap before GC
                                    , Size -- The size of the heap after GC
                                    )]
             }
  deriving (Eq, Show)

statInitial :: TimStats
statInitial = TimStats { getSteps = 0
                       , getExecTime = 0
                       , getHeapAllocated = 0
                       , getMaxStackDepth = 0
                       , getGCInfo = []
                       }

statGetSteps :: TimStats -> Int
statGetSteps s = getSteps s

statSetSteps :: Int -> TimStats -> TimStats
statSetSteps n sts = sts { getSteps = n }

statIncSteps :: TimStats -> TimStats
statIncSteps sts = sts { getSteps = getSteps sts + 1 }

statGetExecTime :: TimStats -> Int
statGetExecTime s = getExecTime s

statIncExecTime :: TimStats -> TimStats
statIncExecTime sts = sts { getExecTime = getExecTime sts + 1 }

statGetHeapAllocated :: TimStats -> Int
statGetHeapAllocated s = getHeapAllocated s

statIncHeapAllocated :: Int -> TimStats -> TimStats
statIncHeapAllocated n sts = sts { getHeapAllocated = getHeapAllocated sts + n }

statGetMaxStackDepth :: TimStats -> Int
statGetMaxStackDepth s = getMaxStackDepth s

statUpdateMaxStackDepth :: Int -> TimStats -> TimStats
statUpdateMaxStackDepth depth s
  | depth > depth' = s { getMaxStackDepth = depth }
  | otherwise      = s
  where depth' = statGetMaxStackDepth s

statGetGCInfo :: TimStats -> [(Int, Size, Size)]
statGetGCInfo s = getGCInfo s

statGetGCCount :: TimStats -> Int
statGetGCCount s = length $ getGCInfo s

statIncGCCount :: (Int, Size, Size) -> TimStats -> TimStats
statIncGCCount tpl sts = sts { getGCInfo = getGCInfo sts ++ [tpl] }

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
initialArgStack = [([], FrameNull)] -- initial continuation

initialValueStack :: TimValueStack
initialValueStack = []

initialDump :: TimDump
initialDump = DummyTimDump

initCodeStore :: CodeStore
initCodeStore = []

data OpType = BinOp Op | UniOp Op | CondOp deriving (Eq, Show)

primitives :: [(Name, OpType)]
primitives = [ ("+",      BinOp Add)
             , ("-",      BinOp Sub)
             , ("*",      BinOp Mul)
             , ("/",      BinOp Div)
             , ("negate", UniOp Neg)
             , ("==",     BinOp Eq)
             , ("/=",     BinOp Ne)
             , ("<",      BinOp Lt)
             , ("<=",     BinOp Le)
             , (">",      BinOp Gt)
             , (">=",     BinOp Ge)
             , ("if",     CondOp)
             ]

compiledPrimitives :: [(Name, CompiledCode)]
compiledPrimitives = map (second trans) primitives
  where trans opt = case opt of
          BinOp op -> Compiled [1, 2] [ Take 2
                                      , Push (Code (Compiled [1] [ Push (Code (Compiled [] [Op op, Return]))
                                                                 , Enter (Arg 1)]))
                                      , Enter (Arg 2)]
          UniOp op -> Compiled [1] [ Take 1
                                   , Push (Code (Compiled [] [Op op, Return]))
                                   , Enter (Arg 1)]
          CondOp   -> Compiled [1, 2, 3] [ Take 3
                                         , Push (Code (Compiled [2, 3] [ Cond [Enter (Arg 2)] [Enter (Arg 3)] ]))
                                         , Enter (Arg 1)]
        
type TimCompilerEnv = [(Name, TimAMode)]

compileSc :: TimCompilerEnv -> CoreScDefn -> (Name, CompiledCode)
compileSc env (name, args, body)
  | null args = (name, cs)
  | otherwise = (name, Compiled ns (Take (length args) : il))
  where
    cs = compileR body new_env
    Compiled ns il = cs
    new_env = zip args (map Arg [1..]) ++ env

compileR :: CoreExpr -> TimCompilerEnv -> CompiledCode
compileR e env = case e of
  EAp e1 e2 | isBasicOp e -> compileB e env (Compiled [] [Return])
            | isCondOp e  -> let (kCond, kThen, kElse) = unpackCondOp e
                                 Compiled ns1 il1 = compileR kThen env
                                 Compiled ns2 il2 = compileR kElse env
                             in compileB kCond env (Compiled (uniq $ ns1 ++ ns2) [Cond il1 il2])
            | otherwise   -> let Compiled ns1 il1 = compileR e1 env
                                 (ns2, arg) = usedSlots &&& id $ compileA e2 env
                             in Compiled (uniq $ ns1 ++ ns2) (Push arg : il1)
  EVar v  -> Compiled ns [Enter amode]
    where (ns, amode) = usedSlots &&& id $ compileA (EVar v) env
  ENum n  -> Compiled [] [PushV (IntVConst n), Return]
  _       -> error $ "compileR: can't do this yet: " ++ show e
  where usedSlots :: TimAMode -> UsedSlots
        usedSlots arg = case arg of
          Arg i   -> [i]
          Code cs -> slotsOf cs -- NOTE: EVar, ENum のときは今のところこれは起きないはず?
          _       -> []
        uniq = nub . sort

compileB :: CoreExpr -> TimCompilerEnv -> CompiledCode -> CompiledCode
compileB e env cont
  | isBinOp e = compileB e2 env (compileB e1 env (Compiled slots' (Op op:cont')))
  where (e1, op, e2) = unpackBinOp e
        Compiled slots' cont' = cont
compileB e env cont
  | isUniOp e = compileB e1 env (Compiled slots' (Op op:cont'))
  where (op, e1) = unpackUniOp e
        Compiled slots' cont' = cont
compileB (ENum n) env cont = Compiled slots' (PushV (IntVConst n):cont')
  where Compiled slots' cont' = cont
compileB e env cont        = Compiled slots' (Push (Code cont):cont')
  where Compiled slots' cont' = compileR e env

isBasicOp :: CoreExpr -> Bool
isBasicOp e = isBinOp e || isUniOp e

isBinOp :: CoreExpr -> Bool
isBinOp (EAp (EAp (EVar op) _) _) = op `elem` basicOps
  where basicOps = map fst $ filter (isBin . snd) primitives
        isBin (BinOp _) = True
        isBin _         = False
isBinOp _                         = False

isUniOp :: CoreExpr -> Bool
isUniOp (EAp (EVar op) _) = op `elem` uniOps
  where uniOps = map fst $ filter (isUni . snd) primitives
        isUni (UniOp _) = True
        isUni _         = False
isUniOp _               = False

isCondOp :: CoreExpr -> Bool
isCondOp (EAp (EAp (EAp (EVar "if") e1) e2) e3) = True
isCondOp _ = False

unpackBinOp :: CoreExpr -> (CoreExpr, Op, CoreExpr)
unpackBinOp (EAp (EAp (EVar op) e1) e2) = (e1, op2binop op, e2)
  where op2binop o = case lookup o primitives of
          Just (BinOp op') -> op'
          _                -> error "unpackBinOp: not a binary operator"
unpackBinOp _                           = error "unpackBinOp: not a binary operator"

unpackUniOp :: CoreExpr -> (Op, CoreExpr)
unpackUniOp (EAp (EVar op) e1) = (op2uniop op, e1)
  where op2uniop o = case lookup o primitives of
          Just (UniOp op') -> op'
          _                -> error "unpackUniOp: not a unary operator"
unpackUniOp _                  = error "unpackUniOp: not a unary operator"

unpackCondOp :: CoreExpr -> (CoreExpr, CoreExpr, CoreExpr)
unpackCondOp (EAp (EAp (EAp (EVar "if") e1) e2) e3) = (e1, e2, e3)
unpackCondOp _                                      = error "unpackCondOp: not a conditional operator"

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
doAdmin state
  | needGC    = applyToStats (statIncGCCount (stepAt, orgSize, newSize)) gcedState
  | otherwise = state'
  where
    state' = applyToStats statIncSteps state
    needGC = orgSize >= threshold
    gcedState = gc state'
    orgSize = hSize $ getHeap state'
    newSize = hSize $ getHeap gcedState
    stepAt = statGetSteps $ getStats state'

gc :: TimState -> TimState
gc state@TimState { instructions = instrs
                  , frame        = fptr
                  , stack        = stk
                  , dump         = dmp
                  , heap         = from
                  , codes        = cstore
                  }
  = case evacuateStack cstore from hInitial stk of
  ((from1, to1), stk1) -> case evacuateDump cstore from1 to1 dmp of
    ((from2, to2), dmp1) -> case evacuateFramePtr True cstore from2 to2  (instrs, fptr) of
      ((from3, to3),  fptr1) -> case scavenge from3 to3 of
        to4 -> trace (gcPrint instrs fptr from from1 to1 from2 to2 from3 to3 to4 fptr1) $
          state { frame = fptr1
                , stack = stk1
                , dump = dmp1
                , heap = to4
                }
  where
    gcPrint is fp f0 f1 t1 f2 t2 f3 t3 t4 fp'
      = iDisplay $ iConcat
      [ iNewline
      , iStr "vvvvvvvvvvvvvvvvvvvvvvvv", iNewline
      , iStr "step: ", iNum (statGetSteps $ getStats state), iNewline
      , iStr "instr: ", iNewline
      , showInstructions Full is, iNewline
      , iStr "frame ptr: ", showFramePtr fp, iNewline
      , iStr "arg stack: ", showStack stk, iNewline
      , iStr "before", iNewline
      , showHeap f0, iNewline
      , iStr "evacuated stack: from1", iNewline
      , showHeap f1, iNewline
      , iStr "evacuated stack: to1", iNewline
      , showHeap t1, iNewline
      , iStr "evacuated dump: from2", iNewline
      , showHeap f2, iNewline
      , iStr "evacuated dump: to2", iNewline
      , showHeap t2, iNewline
      , iStr "evacuated frameptr: from3", iNewline
      , showHeap f3, iNewline
      , iStr "evacuated frameptr: to3", iNewline
      , showHeap t3, iNewline
      , iStr "scavenged: to4", iNewline
      , showHeap t4, iNewline
      , iStr "new frame ptr: ", showFramePtr fp', iNewline
      , iStr "^^^^^^^^^^^^^^^^^^^^^^^^", iNewline
      ]

-- | NOTE: Closure = ([Instruction], FramePtr) なので
-- [Instruction] の中で使われる FramePtr はコピーし、それ以外は ([], FrameNull) で潰す
{- |
>>> let h = hInitial :: Heap Frame
>>> let cs = initCodeStore
>>> let (h1, a1) = hAlloc h (Frame [([Push (Arg 1)], FrameNull)])
>>> let (h2, a2) = hAlloc h1 (Frame [([Push (Arg 2)], FrameAddr 1)])
>>> let ((from, to), fp) = evacuateFramePtr True cs h2 hInitial ([Push (Arg 1)], FrameAddr 2)
>>> from
(2,2,[(1,Forward 2),(2,Forward 1)])
>>> to
(2,2,[(1,Frame [([Push (Arg 2)],FrameAddr 1)]),(2,Frame [([Push (Arg 1)],FrameNull)])])
>>> fp
FrameAddr 1

>>> let (h3, a3) = hAlloc h (Frame [([Push (Arg 1)], FrameNull),([Push (Arg 2)], FrameInt 42),([Push (Arg 3)], FrameAddr 2)])
>>> let (h4, a4) = hAlloc h3 (Frame [([Push (Arg 1)], FrameAddr 1)])
>>> let ((from2, to2), fp2) = evacuateFramePtr True cs h4 hInitial ([Push (Arg 1)], FrameAddr 2)
>>> from2
(2,2,[(1,Forward 2),(2,Forward 1)])
>>> to2
(2,2,[(1,Frame [([Push (Arg 1)],FrameAddr 1)]),(2,Frame [([Push (Arg 1)],FrameNull),([Push (Arg 2)],FrameInt 42),([Push (Arg 3)],FrameAddr 2)])])
>>> fp2
FrameAddr 1

>>> let ((from3, to3), fp3) = evacuateFramePtr True cs h4 hInitial ([Push (Arg 2)], FrameAddr 2)
>>> from3
(2,2,[(2,Forward 1),(1,Frame [([Push (Arg 1)],FrameNull),([Push (Arg 2)],FrameInt 42),([Push (Arg 3)],FrameAddr 2)])])
>>> to3
(1,1,[(1,Frame [([],FrameNull)])])
>>> fp3
FrameAddr 1

>>> let ((from4, to4), fp4) = evacuateFramePtr True cs h4 hInitial ([Push (Arg 1)], FrameAddr 1)
>>> from4
(2,2,[(1,Forward 1),(2,Frame [([Push (Arg 1)],FrameAddr 1)])])
>>> to4
(1,1,[(1,Frame [([Push (Arg 1)],FrameNull),([],FrameNull),([],FrameNull)])])
>>> fp4
FrameAddr 1

>>> let ((from5, to5), fp5) = evacuateFramePtr True cs h4 hInitial ([Push (Arg 2)], FrameAddr 1)
>>> from5
(2,2,[(1,Forward 1),(2,Frame [([Push (Arg 1)],FrameAddr 1)])])
>>> to5
(1,1,[(1,Frame [([],FrameNull),([Push (Arg 2)],FrameInt 42),([],FrameNull)])])
>>> fp5
FrameAddr 1

>>> let ((from6, to6), fp6) = evacuateFramePtr True cs h4 hInitial ([Push (Arg 3)], FrameAddr 1)
>>> from6
(2,2,[(2,Forward 2),(1,Forward 1)])
>>> to6
(2,2,[(1,Frame [([],FrameNull),([],FrameNull),([Push (Arg 3)],FrameAddr 2)]),(2,Frame [([Push (Arg 1)],FrameAddr 1)])])
>>> fp6
FrameAddr 1

>>> let ((from7, to7), fp7) = evacuateFramePtr True cs h4 hInitial ([Push (Arg 1), Enter (Arg 3)], FrameAddr 1)
>>> from7
(2,2,[(2,Forward 2),(1,Forward 1)])
>>> to7
(2,2,[(1,Frame [([Push (Arg 1)],FrameNull),([],FrameNull),([Push (Arg 3)],FrameAddr 2)]),(2,Frame [([Push (Arg 1)],FrameAddr 1)])])
>>> fp7
FrameAddr 1
-}
{- | The case for Cyclic Reference
>>> let h = hInitial :: Heap Frame
>>> let cs = initCodeStore
>>> let (h1, a1) = hAlloc h (Frame [([Push (Arg 1)], FrameAddr 2)])
>>> let (h2, a2) = hAlloc h1 (Frame [([Push (Arg 1)], FrameAddr 1)])
>>> let ((from, to), fp) = evacuateFramePtr True cs h2 hInitial ([Push (Arg 1)], FrameAddr 2)
>>> from
(2,2,[(1,Forward 2),(2,Forward 1)])
>>> to
(2,2,[(1,Frame [([Push (Arg 1)],FrameAddr 1)]),(2,Frame [([Push (Arg 1)],FrameAddr 2)])])
>>> fp
FrameAddr 1

>>> let (h3, a3) = hAlloc h2 (Frame [([Push (Arg 1)], FrameAddr 2)])
>>> h3
(3,3,[(3,Frame [([Push (Arg 1)],FrameAddr 2)]),(2,Frame [([Push (Arg 1)],FrameAddr 1)]),(1,Frame [([Push (Arg 1)],FrameAddr 2)])])
>>> let ((from', to'), fp') = evacuateFramePtr True cs h3 hInitial ([Push (Arg 1)], FrameAddr 3)
>>> from'
(3,3,[(1,Forward 3),(2,Forward 2),(3,Forward 1)])
>>> to'
(3,3,[(1,Frame [([Push (Arg 1)],FrameAddr 2)]),(2,Frame [([Push (Arg 1)],FrameAddr 1)]),(3,Frame [([Push (Arg 1)],FrameAddr 2)])])

>>> let ((from'', to''), fp'') = evacuateFramePtr True cs h3 hInitial ([Push (Arg 1)], FrameAddr 2)
>>> from''
(3,3,[(1,Forward 2),(2,Forward 1),(3,Frame [([Push (Arg 1)],FrameAddr 2)])])
>>> to''
(2,2,[(1,Frame [([Push (Arg 1)],FrameAddr 1)]),(2,Frame [([Push (Arg 1)],FrameAddr 2)])])
-}
{- | The case for Self-Cyclic Reference
>>> let h = hInitial :: Heap Frame
>>> let cs = initCodeStore
>>> let (h1, a1) = hAlloc h (Frame [([Push (Arg 1)], FrameAddr 2)])
>>> let (h2, a2) = hAlloc h1 (Frame [([Push (Arg 1)], FrameAddr 2)]) -- self-cyclic reference
>>> let ((from, to), fp) = evacuateFramePtr True cs h2 hInitial ([Push (Arg 1)], FrameAddr 1)
>>> from
(2,2,[(2,Forward 2),(1,Forward 1)])
>>> to
(2,2,[(1,Frame [([Push (Arg 1)],FrameAddr 2)]),(2,Frame [([Push (Arg 1)],FrameAddr 2)])])
>>> fp
FrameAddr 1

>>> let ((from', to'), fp') = evacuateFramePtr True cs h2 hInitial ([Push (Arg 1)], FrameAddr 2)
>>> from'
(2,2,[(2,Forward 1),(1,Frame [([Push (Arg 1)],FrameAddr 2)])])
>>> to'
(1,1,[(1,Frame [([Push (Arg 1)],FrameAddr 2)])])
>>> fp'
FrameAddr 1
-}
{- | check CodeStore
>>> let h = hInitial :: Heap Frame
>>> let cs = [("f", Compiled [1,3] [Push (Arg 1), Enter (Arg 3)])]
>>> let (h1, a1) = hAlloc h (Frame [([Take 1], FrameNull),([Take 2],FrameNull),([Take 3],FrameNull)])
>>> let (h2, a2) = hAlloc h1 (Frame [([Enter (Label "f")], FrameAddr 1)])
>>> let ((from, to), fp) = evacuateFramePtr True cs h2 hInitial ([Push (Arg 1)], FrameAddr 2)
>>> from
(2,2,[(1,Forward 2),(2,Forward 1)])
>>> to
(2,2,[(1,Frame [([Enter (Label "f")],FrameAddr 1)]),(2,Frame [([Take 1],FrameNull),([Take 2],FrameNull),([Take 3],FrameNull)])])
>>> fp
FrameAddr 1
-}
{- | check Code
>>> let h = hInitial :: Heap Frame
>>> let (h1, a1) = hAlloc h (Frame [([Take 1], FrameNull),([Take 2],FrameNull),([Take 3],FrameNull)])
>>> let (h2, a2) = hAlloc h1 (Frame [([Enter (Code (Compiled [1,3] [Push (Arg 1), Enter (Arg 3)]))], FrameAddr 1)])
>>> let ((from, to), fp) = evacuateFramePtr True initCodeStore h2 hInitial ([Push (Arg 1)], FrameAddr 2)
>>> from
(2,2,[(1,Forward 2),(2,Forward 1)])
>>> to
(2,2,[(1,Frame [([Enter (Code (Compiled {slotsOf = [1,3], instrsOf = [Push (Arg 1),Enter (Arg 3)]}))],FrameAddr 1)]),(2,Frame [([Take 1],FrameNull),([Take 2],FrameNull),([Take 3],FrameNull)])])
>>> fp
FrameAddr 1
-}
evacuateFramePtr :: Bool -> CodeStore -> TimHeap -> TimHeap -> ([Instruction], FramePtr) -> ((TimHeap, TimHeap), FramePtr)
evacuateFramePtr liveCheck cstore from to (instrs, fptr) = case fptr of
  FrameAddr a -> case hLookup from a of
    Frame clss -> case hAlloc to (Frame []) of
      (to', a') -> case hUpdate from a (Forward a') of
        from' -> case mapAccumL update (from', to') (zip [1..] clss) of
          ((from'', to''), clss') -> case hUpdate to'' a' (Frame clss') of
            to''' -> ((from'', to'''), FrameAddr a')

    -- すでに置き換え済
    Forward a'  -> ((from, to), FrameAddr a')
    where
      update :: (TimHeap, TimHeap) -> (Int, Closure) -> ((TimHeap, TimHeap), Closure)
      update (f, t) (i, cls@(is, fp))
        | not liveCheck || i `elem` liveArgs = case evacuateFramePtr False cstore f t cls of
            (hs, _) -> (hs, (is, fp)) -- NOTE: ここで fp' としない (scavenge がやる)
        | otherwise         = ((f, t), ([], FrameNull))
      liveArgs :: [Int]
      liveArgs = nub $ foldl g [] instrs
        where
          g ns (Push am)  = h ns am
          g ns (Enter am) = h ns am
          g ns _          = ns

          h ns (Arg n)   = n:ns
          h ns (Code cs) = slotsOf cs ++ ns
          h ns (Label l) = ns
          h ns _         = ns

              
  -- Heap には含まないので from と to で変わらない
  FrameInt n -> ((from, to), fptr)
  FrameNull  -> ((from, to), fptr)

{- |
>>> let h = hInitial :: Heap Frame
>>> let cs = initCodeStore
>>> let (h1, a1) = hAlloc h (Frame [([Push (Arg 1)], FrameNull)])
>>> let (h2, a2) = hAlloc h1 (Frame [([Push (Arg 2)], FrameAddr 1)])
>>> let ((from, to), stk) = evacuateStack cs h2 hInitial [([Push (Arg 1)], FrameAddr 2), ([Push (Arg 2)], FrameAddr 1)]
>>> from
(2,2,[(1,Forward 2),(2,Forward 1)])
>>> to
(2,2,[(1,Frame [([Push (Arg 2)],FrameAddr 1)]),(2,Frame [([Push (Arg 1)],FrameNull)])])
>>> stk
[([Push (Arg 1)],FrameAddr 1),([Push (Arg 2)],FrameAddr 2)]
-}
evacuateStack :: CodeStore -> TimHeap -> TimHeap -> TimStack -> ((TimHeap, TimHeap), TimStack)
evacuateStack cstore from to stk = case mapAccumL update (from, to) stk of
  (hs,fps) -> (hs, zipWith (\(is, _) fp -> (is, fp)) stk fps)
  where
    update :: (TimHeap, TimHeap) -> ([Instruction], FramePtr) -> ((TimHeap, TimHeap), FramePtr)
    update (f, t) (is, fp) = evacuateFramePtr False cstore f t (is, fp)

-- | NOTE: Dump はまだ使われてないので id 的な実装になっている
evacuateDump :: CodeStore -> TimHeap -> TimHeap -> TimDump -> ((TimHeap, TimHeap), TimDump)
evacuateDump cstore from to dump = ((from, to), dump)

-- | 新しいヒープ中の FramePtr を 古いヒープから探して、
--   新しいヒープのどのアドレスに Forward されているか見て付け替えていく
{- |
>>> let from = (2,2,[3..],[(1,Forward 2),(2,Forward 1)])
>>> let to = (2,2,[3..],[(1,Frame [([Push (Arg 2)],FrameAddr 1)]),(2,Frame [([Push (Arg 1)],FrameNull)])])
>>> scavenge from to
(2,2,[(2,Frame [([Push (Arg 1)],FrameNull)]),(1,Frame [([Push (Arg 2)],FrameAddr 2)])])
-}
scavenge :: TimHeap -> TimHeap -> TimHeap
scavenge from to@(_, _, _, hp) = foldl phi to hp
  where
    phi :: TimHeap -> (Addr, Frame) -> TimHeap
    phi t (a', f) = case f of
      Frame cls -> hUpdate t a' (Frame $ map conv cls)
        where
          conv :: Closure -> Closure
          conv cls@(is, fp) = case fp of
            FrameAddr a -> case hLookup from a of
              Forward a' -> (is, FrameAddr a')
              _          -> error $ "scavenge: not Forward: " ++ show cls
            FrameInt _  -> cls
            FrameNull   -> cls
      Forward _ -> error $ "scavenge: found Forward in new heap: " ++ show f

timFinal :: TimState -> Bool
timFinal state = null $ instructions state

applyToStats :: (TimStats -> TimStats) -> TimState -> TimState
applyToStats stats_fun state
  = state { stats = stats_fun $ stats state }

step :: TimState -> TimState
step state@TimState { instructions = instrs
                    , frame        = fptr
                    , stack        = stk
                    , valstack     = vstk
                    , heap         = hp
                    , codes        = cstore
                    }
  = case instrs of
  (Take n:instr)
    | length stk >= n
      -- NOTE: Take は exec time にカウントしない (exercise 4.2)
      -> applyToStats (statIncHeapAllocated $ n + 1)
         (putInstructions instr
          . putFrame fptr'
          . putStack stk'
          . putHeap hp'
          $ state)
    | otherwise -> error "Too few args for Take instruction"
    where
      (hp', fptr') = fAlloc hp (Frame $ take n stk)
      stk' = drop n stk
  [Enter am]
    -> applyToStats statIncExecTime
       (putInstructions instr'
        . putFrame fptr'
        $ state)
    where
      (instr', fptr') = amToClosure am fptr hp cstore
  (Push am:istr)
    -> applyToStats statIncExecTime
       (putInstructions istr
         . putStack (amToClosure am fptr hp cstore : stk)
         $ state)
  (PushV fp:istr)
    -> applyToStats statIncExecTime $ case fp of
      FramePtr -> putInstructions istr
                  . putVStack (n:vstk)
                  $ state
        where n = case fptr of
                FrameInt n' -> n'
                _           -> error "PushV applied to non-int frame"
      IntVConst n -> putInstructions istr
                     . putVStack (n:vstk)
                     $ state
  [Return]
    -> applyToStats statIncExecTime
       (putInstructions instr'
        . putFrame fptr'
        . putStack stk'
        $ state)
    where
      ((instr', fptr'), stk') = case stk of
        [] -> error "Return applied to empty stack"
        (i, f):s -> ((i, f), s)
  (Op op:istr)
    -> applyToStats statIncExecTime
       (putInstructions istr
        . putVStack vstk'
        $ state)
    where
      vstk'
        | op `elem` [Add, Sub, Mul, Div, Eq, Ne, Lt, Le, Gt, Ge] = case vstk of
            (n1:n2:ns) -> op' n1 n2:ns
            _          -> error "Binary op applied to empty stack"
        | op == Neg = case vstk of
            (n:ns) -> negate n:ns
            _      -> error "Unary op applied to empty stack"
      op' = case op of
        Add -> (+)
        Sub -> (-)
        Mul -> (*)
        Div -> div
        Eq  -> b2i (==)
        Ne  -> b2i (/=)
        Lt  -> b2i (<)
        Le  -> b2i (<=)
        Gt  -> b2i (>)
        Ge  -> b2i (>=)
        _   -> error $ "unsupported op: " ++ show op
        where
          b2i cmp x y = if x `cmp` y then 0 else 1
  [Cond i1 i2]
    -> applyToStats statIncExecTime
       (putInstructions instr'
       . putVStack vstk'
       $ state)
    where
      (instr', vstk') = case vstk of
        (0:ns) -> (i1, ns)
        (_:ns) -> (i2, ns)
        _      -> error "Cond applied to empty stack"
  _ -> error $ "invalid instructions: " ++ show instrs


amToClosure :: TimAMode -> FramePtr -> TimHeap -> CodeStore -> Closure
amToClosure (Arg n)      fptr heap cstore = fGet heap fptr n
amToClosure (Code cs)    fptr heap cstore = (instrsOf cs, fptr)
amToClosure (Label l)    fptr heap cstore = (codeLookup cstore l, fptr)
amToClosure (IntConst n) fptr heap cstore = (intCode, FrameInt n)

intCode :: [Instruction]
intCode = [PushV FramePtr, Return]

showSimpleResult :: [TimState] -> String
showSimpleResult states = iDisplay $ showVStackTopValue vs
  where last_state = last states
        vs = getVStack last_state

showResults :: [TimState] -> String
showResults [] = error "no TimState"
showResults states@(s:ss)
  = unlines (map iDisplay
             ([ iStr "Supercombinator definitions", iNewline, iNewline
              , showSCDefns s, iNewline, iNewline
              , iStr "State transitions", iNewline
              ] ++
              iLayn' (map showState states) ++
              [ showStats (last states)
              ])
            )

showSCDefns :: TimState -> IseqRep
showSCDefns state@TimState { codes = cstore }
  = iInterleave iNewline (map showSC cstore)

showSC :: (Name, CompiledCode) -> IseqRep
showSC (name, cs)
  = iConcat [ iStr "Code for ", iStr name, iStr ":", iNewline
            , iStr "   ", showUsedSlots ns, iNewline
            , iStr "   ", showInstructions Full instrs, iNewline, iNewline
            ]
    where Compiled ns instrs = cs

showState :: TimState -> IseqRep
showState state@TimState { instructions = instr
                         , frame        = fptr
                         , stack        = stk
                         , valstack     = vstk
                         , dump         = dmp
                         , heap         = hp
                         , codes        = cstore
                         , stats        = stat
                         }
  = iConcat [ iStr "Code:  "
            , showInstructions Terse instr, iNewline
            , showFrame hp fptr
            , showStack stk
            , showValueStack vstk
            , showDump dmp
            , iNewline
            ]

showUsedSlots :: [Int] -> IseqRep
showUsedSlots ns = iConcat [ iStr "["
                           , iInterleave (iStr ",") (map iNum ns)
                           , iStr "]"
                           ]

showInstructions :: HowMuchToPrint -> [Instruction] -> IseqRep
showInstructions None il = iStr "{..}"
showInstructions Terse il
  = iConcat [ iStr "{"
            , iIndent (iInterleave (iStr ", ") body)
            , iStr "}"
            ]
  where
    instrs = map (showInstruction None) il
    body | length il <= nTerse = instrs
         | otherwise           = take nTerse instrs ++ [iStr ".."]
showInstructions Full il
  = iConcat [iStr "{ ", iIndent (iInterleave sep instrs), iStr " }"]
  where
    instrs = map (showInstruction Full) il
    sep = iStr "," `iAppend` iNewline

showInstruction :: HowMuchToPrint -> Instruction -> IseqRep
showInstruction d (Take n)   = iStr "Take " `iAppend` iNum n
showInstruction d (Enter x)  = iStr "Enter " `iAppend` showArg d x
showInstruction d (Push x)   = iStr "Push " `iAppend` showArg d x
showInstruction d (PushV x)  = iStr "PushV " `iAppend` showValueAMode x
showInstruction d Return     = iStr "Return"
showInstruction d (Op op)    = iStr "Op " `iAppend` iStr (show op)
showInstruction d (Cond t f) = iConcat [ iStr "Cond "
                                       , iIndent (iConcat [ showInstructions d t
                                                          , iNewline
                                                          , showInstructions d f
                                                          ])
                                       ]

showValueAMode :: ValueAMode -> IseqRep
showValueAMode FramePtr      = iStr "FramePtr"
showValueAMode (IntVConst n) = iStr "IntVConst " `iAppend` iNum n

showArg :: HowMuchToPrint -> TimAMode -> IseqRep
showArg d (Arg n)   = iStr "Arg " `iAppend` iNum n
showArg d (Code il) = iConcat [ iStr "Code "
                              , showUsedSlots ns
                              , iStr " "
                              , showInstructions d instrs
                              ]
  where Compiled ns instrs = il
showArg d (Label s) = iStr "Label " `iAppend` iStr s
showArg d (IntConst n) = iStr "IntConst " `iAppend` iNum n

nTerse :: Int
nTerse = 3

showFrame :: TimHeap -> FramePtr -> IseqRep
showFrame heap FrameNull = iStr "Null frame ptr" `iAppend` iNewline
showFrame heap (FrameAddr addr)
  = iConcat [ iStr "Frame: <"
            , iIndent (either showAddr (iInterleave iNewline . map showClosure) (fList (hLookup heap addr)))
            , iStr ">", iNewline
            ]
  where
    showAddr :: Addr -> IseqRep
    showAddr a = iStr "FW #->" `iAppend` iNum a
showFrame heap (FrameInt n)
  = iConcat [ iStr "Frame ptr (int): ", iNum n, iNewline ]

showHeap :: TimHeap -> IseqRep
showHeap heap@(_, _, _, hp)
  = iConcat [ iStr "Heap: ["
            , iIndent (iInterleave iNewline $ map showHeapItem hp)
            , iStr "]"
            ]
  where
    showHeapItem :: (Addr, Frame) -> IseqRep
    showHeapItem (addr, fr)
      = iConcat [ showFWAddr addr, iStr ": "
                , showFrame heap (FrameAddr addr)
                ]

showFWAddr :: Addr -> IseqRep
showFWAddr addr = iStr (space (4 - length str) ++ str)
  where str = show addr

showStack :: TimStack -> IseqRep
showStack stack
  = iConcat [ iStr "Arg stack: ["
            , iIndent (iInterleave iNewline (map showClosure stack))
            , iStr "]", iNewline
            ]

showValueStack :: TimValueStack -> IseqRep
showValueStack vstack = iConcat [ iStr "Value stack: ["
                                , iIndent (iInterleave (iStr ",") (map iNum vstack))
                                , iStr "]", iNewline
                                ]

showVStackTopValue :: TimValueStack -> IseqRep
showVStackTopValue []            = error "empty value stack"
showVStackTopValue vstack@(v:[]) = iNum v
showVStackTopValue vstack@(v:vs) = error $ "value stack has more than 1 value: " ++ show vstack

showDump :: TimDump -> IseqRep
showDump dump = iNil

showClosure :: Closure -> IseqRep
showClosure (i, f)
  = iConcat [ iStr "("
            , showInstructions Terse i
            , iStr ", "
            , showFramePtr f, iStr ")"
            ]

showFramePtr :: FramePtr -> IseqRep
showFramePtr FrameNull     = iStr "null"
showFramePtr (FrameAddr a) = iStr "#" `iAppend` iNum a
showFramePtr (FrameInt n)  = iStr "int " `iAppend` iNum n

showGCInfo :: [(Int, Size, Size)] -> IseqRep
showGCInfo xs | null xs   = iConcat [ iNum 0, iNewline ]
              | otherwise = iConcat [ iNum (length xs)
                                    , iStr " { "
                                    , iIndent (iInterleave iNewline $ map showResize xs)
                                    , iStr " }"
                                    ]
  where showResize (n, f, t) = iConcat [ iNum n, iStr ") ", iNum f, iStr " -> ", iNum t ]

showStats :: TimState -> IseqRep
showStats state@TimState { stats = stats }
  = iConcat [ iStr "    Steps taken = ", iNum (statGetSteps stats), iNewline
            , iStr "      Exec time = ", iNum (statGetExecTime stats), iNewline
            , iStr " Heap allocated = ", iNum (statGetHeapAllocated stats), iNewline
            , iStr "Max stack depth = ", iNum (statGetMaxStackDepth stats), iNewline
            , iStr "        GC call = ", showGCInfo (statGetGCInfo stats), iNewline
            ]

data HowMuchToPrint = None
                    | Terse
                    | Full
                    deriving (Eq, Show)
