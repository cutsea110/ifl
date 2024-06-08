{-# LANGUAGE FlexibleInstances #-}
module TIM.Mark4
  ( parse
  , compile
  , eval
  , showResults
  , runProg
  , Config(..)
  ) where

import Control.Arrow (second)
import Data.List (find, foldl', mapAccumL, nub, sort)

import Heap
import Iseq
import Language
import Utils

data Config = Config { verbose     :: !Bool
                     , gcThreshold :: !Int
                     }

runProg :: Config -> String -> String
runProg conf = showR . eval conf . compile . parse
  where showR | verbose conf = showResults
              | otherwise    = showSimpleResult

data Instruction = Take Int Int       -- take t n
                 | Move Int TimAMode
                 | Enter TimAMode
                 | Push TimAMode
                 | PushV ValueAMode
                 | PushMarker Int
                 | Return
                 | Op Op
                 | Cond [Instruction] [Instruction]
                 deriving (Eq, Show)

type OccupiedSlotIdx = Int
type UsedSlot  = Int
type UsedSlots = [UsedSlot]
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
type TimDump = [(FramePtr, -- The frame to be updated
                 Int,      -- Index of slot to be updated
                 TimStack  -- Old stack
                )]
type TimHeap = Heap Frame
instance {-# Overlapping #-} Show (Heap Frame) where
  -- NOTE: addr field is infinite list, so we shouldn't show it.
  show (allocs, size, _, cts) = show (allocs, size, cts)

type RelSlot = Assoc UsedSlot UsedSlots

data Frame = Frame [Closure] RelSlot
           | Forward Addr
           deriving (Eq, Show)

fAlloc :: TimHeap -> Frame -> (TimHeap, FramePtr)
fAlloc heap xs = (heap', FrameAddr addr)
  where
    (heap', addr) = hAlloc heap xs

fGet :: TimHeap -> FramePtr -> Int -> Closure
fGet heap (FrameAddr addr) n = case frm of
  Frame f _    -> f !! (n-1)
  Forward addr -> error $ "fGet: Unexpected " ++ show frm
  where
    frm = hLookup heap addr
fGet _ _ _ = error "fGet: not implemented"

fRelSlots :: Frame -> RelSlot
fRelSlots (Frame _ m) = m
fRelSlots (Forward _) = []

fSetRelSlots :: TimHeap -> FramePtr -> (UsedSlot, UsedSlots) -> TimHeap
fSetRelSlots heap (FrameAddr addr) (key, val) = hUpdate heap addr new_frame
  where
    (cs, m) = case hLookup heap addr of
      Frame cs m -> (cs, m)
      frm        -> error $ "fSetRelSlots: Unexpected " ++ show frm
    new_frame = Frame cs (aUpdate m key val)
fSetRelSlots _ _ _ = error "fSetRelSlots: not implemented"

fUpdate :: TimHeap -> FramePtr -> Int -> Closure -> TimHeap
fUpdate heap (FrameAddr addr) n closure = hUpdate heap addr new_frame
  where
    (cs, m) = case frm of
      Frame cs m   -> (cs, m)
      Forward addr -> error $ "fUpdate: Unexpected " ++ show frm
      where
        frm = hLookup heap addr
    new_frame = Frame (take (n-1) cs ++ [closure] ++ drop n cs) m
fUpdate _ _ _ _ = error "fUpdate: not implemented"

fList :: Frame -> Either Addr [Closure]
fList (Frame f _) = Right f
fList (Forward a) = Left a

type CodeStore = Assoc Name CompiledCode

codeLookup :: CodeStore -> Name -> [Instruction]
codeLookup cstore l = instrsOf cs
  where cs = aLookup cstore l $ error $ "Attempt to jump to unknown label " ++ show l

data GCInfo = GCInfo { stepAt                  :: Int
                     , instr                   :: [Instruction]
                     , stackInit               :: TimStack
                     , fptrInit                :: FramePtr
                     , heapBefore              :: TimHeap
                     , heapEvacuatedByStack    :: (TimHeap, TimHeap)
                     , heapEvacuatedByDump     :: (TimHeap, TimHeap)
                     , heapEvacuatedByFramePtr :: (TimHeap, TimHeap)
                     , heapScavenged           :: TimHeap
                     , fptrDone                :: FramePtr
                     } deriving (Eq, Show)

data TimStats
  = TimStats { getSteps         :: Int  -- The number of steps
             , getExecTime      :: Int  -- The execution time
             , getHeapAllocated :: Int  -- The amount of heap allocated
             , getMaxStackDepth :: Int  -- The maximum stack depth
             , getGCInfo        :: [GCInfo]
             }
  deriving (Eq, Show)

statInitial :: TimStats
statInitial = TimStats { getSteps         = 0
                       , getExecTime      = 0
                       , getHeapAllocated = 0
                       , getMaxStackDepth = 0
                       , getGCInfo        = []
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

statGetGCInfo :: TimStats -> [GCInfo]
statGetGCInfo s = getGCInfo s

statGetGCCount :: TimStats -> Int
statGetGCCount s = length $ getGCInfo s

statIncGCCount :: GCInfo -> TimStats -> TimStats
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
    initial_env = [(name, Label name) | (name, _, _) <- sc_defs] ++
                  [(name, Label name) | (name, _) <- compiledPrimitives]

initialArgStack :: TimStack
initialArgStack = [([], FrameNull)] -- initial continuation

initialValueStack :: TimValueStack
initialValueStack = []

initialDump :: TimDump
initialDump = []

initCodeStore :: CodeStore
initCodeStore = []

data OpType = BinOp Op | UniOp Op | CondOp deriving (Eq, Show)

isBin :: OpType -> Bool
isBin (BinOp _) = True
isBin _         = False
isUni :: OpType -> Bool
isUni (UniOp _) = True
isUni _         = False

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
          BinOp op -> Compiled [1, 2] [ Take 2 2
                                      , Push (Code (Compiled [1] [ Push (Code (Compiled [] [Op op, Return]))
                                                                 , Enter (Arg 1)]))
                                      , Enter (Arg 2)]
          UniOp op -> Compiled [1] [ Take 1 1
                                   , Push (Code (Compiled [] [Op op, Return]))
                                   , Enter (Arg 1)]
          CondOp   -> Compiled [1, 2, 3] [ Take 3 3
                                         , Push (Code (Compiled [2, 3] [ Cond [Enter (Arg 2)] [Enter (Arg 3)] ]))
                                         , Enter (Arg 1)]
        
type TimCompilerEnv = [(Name, TimAMode)]

compileSc :: TimCompilerEnv -> CoreScDefn -> (Name, CompiledCode)
compileSc env (name, args, body)
  | d' == 0   = (name, cs)
  | otherwise = (name, Compiled ns (Take d' n : il))
  where
    n = length args
    (d', cs@(Compiled ns il)) = compileR body new_env n
    new_env = zip args (map mkUpdIndMode [1..]) ++ env

compileR :: CoreExpr -> TimCompilerEnv -> OccupiedSlotIdx -> (OccupiedSlotIdx, CompiledCode)
compileR e env d = case e of
  EAp e1 e2 | isBasicOp e -> compileB e env (d, Compiled [] [Return])
            -- exercise 4.7
            | isCondOp e  -> let (kCond, kThen, kElse) = unpackCondOp e
                                 (d1, Compiled ns1 il1) = compileR kThen env d
                                 (d2, Compiled ns2 il2) = compileR kElse env d
                                 d' = max d1 d2
                             in compileB kCond env (d', Compiled (merge ns1 ns2) [Cond il1 il2])
            | otherwise   -> let (d1, am) = compileA e2 env d
                                 (d2, Compiled ns2 il2) = compileR e1 env d1
                                 ns1 = usedSlots am
                             in (d2, Compiled (merge ns1 ns2) (Push am : il2))
  ELet isrec defns body -> (d', Compiled (merge ns ns') (moves ++ il))
    where
      n = length defns
      (dn, ams) = mapAccumL (\ix (_, e') -> compileA e' env' ix) (d+n) defns
      env'     | isrec     = let_env
               | otherwise = env
      let_env = zip (map fst defns) (map mkUpdIndMode [d+1..d+n]) ++ env
      (d', Compiled ns il) = compileR body let_env dn
      moves = zipWith Move [d+1..d+n] ams
      ns' = nub . sort $ concatMap usedSlots ams -- moves で使われているスロット
  EVar v -> (d', Compiled ns [Enter am])
    where (d', am) = compileA (EVar v) env d
          ns = usedSlots am
  ENum n -> (d, Compiled [] [PushV (IntVConst n), Return])
  _      -> error $ "compileR: can't do this yet: " ++ show e
  where usedSlots (Arg i)   = [i]
        usedSlots (Code cs) = slotsOf cs -- NOTE: EVar, ENum のときは今のところこれは起きないはず?
        usedSlots _         = []
        merge a b = nub . sort $ a ++ b

mkUpdIndMode :: Int -> TimAMode
mkUpdIndMode n = Code (Compiled [n] [PushMarker n, Enter (Arg n)])

compileB :: CoreExpr -> TimCompilerEnv -> (OccupiedSlotIdx, CompiledCode) -> (OccupiedSlotIdx, CompiledCode)
compileB e env (d, Compiled slots cont)
  | isBinOp e = (max d1 d2, Compiled (merge slots1 slots2) il2)
  where (e1, op, e2) = unpackBinOp e
        (d1, am1@(Compiled slots1 _))  = compileB e1 env (d, Compiled slots (Op op : cont))
        (d2,      Compiled slots2 il2) = compileB e2 env (d, am1)
        merge a b = nub . sort $ a ++ b
compileB e env (d, Compiled slots cont)
  | isUniOp e = compileB e1 env (d, Compiled slots (Op op : cont))
  where (op, e1) = unpackUniOp e
compileB (ENum n) env (d, Compiled slots cont) = (d, Compiled slots (PushV (IntVConst n) : cont))
compileB e env (d, cont@(Compiled slots _)) = (d', Compiled (merge slots slots') (Push (Code cont) : cont'))
  where (d', Compiled slots' cont') = compileR e env d
        merge a b = nub . sort $ a ++ b

isBasicOp :: CoreExpr -> Bool
isBasicOp e = isBinOp e || isUniOp e

isBinOp :: CoreExpr -> Bool
isBinOp (EAp (EAp (EVar op) _) _) = op `elem` basicOps
  where basicOps = map fst $ filter (isBin . snd) primitives
isBinOp _                         = False

isUniOp :: CoreExpr -> Bool
isUniOp (EAp (EVar op) _) = op `elem` uniOps
  where uniOps = map fst $ filter (isUni . snd) primitives
isUniOp _                 = False

isCondOp :: CoreExpr -> Bool
isCondOp (EAp (EAp (EAp (EVar "if") e1) e2) e3) = True
isCondOp _                                      = False

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

compileA :: CoreExpr -> TimCompilerEnv -> OccupiedSlotIdx -> (OccupiedSlotIdx, TimAMode)
compileA (EVar v) env d = (d, aLookup env v $ error $ "Unknown variable " ++ v)
compileA (ENum n) env d = (d, IntConst n)
compileA e        env d = (d', Code il)
  where (d', il) = compileR e env d

eval :: Config -> TimState -> [TimState]
eval conf state = state : rest_states
  where rest_states | timFinal state = []
                    | otherwise      = eval conf next_state
        next_state = doAdmin conf $ step state

doAdmin :: Config -> TimState -> TimState
doAdmin conf state
  | needGC    = gc state'
  | otherwise = state'
  where
    state' = applyToStats statIncSteps state
    needGC = orgSize >= gcThreshold conf
    orgSize = hSize $ getHeap state'

gc :: TimState -> TimState
gc state@TimState { instructions = instrs
                  , frame        = fptr
                  , stack        = stk
                  , dump         = dmp
                  , heap         = from
                  , codes        = cstore
                  , stats        = sts
                  }
  = case evacuateStack cstore from hInitial stk of
  ((from1, to1), stk1) -> case evacuateDump cstore from1 to1 dmp of
    ((from2, to2), dmp1) -> case evacuateFramePtr True cstore from2 to2  (instrs, fptr) of
      ((from3, to3),  fptr1) -> case scavenge from3 to3 of
        to4 -> let gcinfo = GCInfo { stepAt = statGetSteps sts
                                   , instr = instrs
                                   , stackInit = stk
                                   , fptrInit = fptr
                                   , heapBefore = from
                                   , heapEvacuatedByStack = (from1, to1)
                                   , heapEvacuatedByDump = (from2, to2)
                                   , heapEvacuatedByFramePtr = (from3, to3)
                                   , heapScavenged = to4
                                   , fptrDone = fptr1
                                   }
               in applyToStats (statIncGCCount gcinfo)
                            $ state { frame = fptr1
                                    , stack = stk1
                                    , dump = dmp1
                                    , heap = to4
                                    }

showGCInfo :: GCInfo -> IseqRep
showGCInfo gcinfo
  = iConcat [ iNewline
            , iStr "vvvvvvvvvvvvvvvvvvvvvvvv", iNewline
            , iStr ">>> BEFORE", iNewline
            , iStr "   ", iIndent (showHeap f0), iNewline, iNewline
            , iStr ">>> EVACUATED stack: from1", iNewline
            , iStr "   ", iIndent (showHeap f1), iNewline, iNewline
            , iStr ">>> EVACUATED stack: to1", iNewline
            , iStr "   ", iIndent (showHeap t1), iNewline, iNewline
            , iStr ">>> EVACUATED dump: from2", iNewline
            , iStr "   ", iIndent (showHeap f2), iNewline, iNewline
            , iStr ">>> EVACUATED dump: to2", iNewline
            , iStr "   ", iIndent (showHeap t2), iNewline, iNewline
            , iStr ">>> EVACUATED frameptr: from3", iNewline
            , iStr "   ", iIndent (showHeap f3), iNewline, iNewline
            , iStr ">>> EVACUATED frameptr: to3", iNewline
            , iStr "   ", iIndent (showHeap t3), iNewline, iNewline
            , iStr ">>> SCAVENGED: to4", iNewline
            , iStr "   ", iIndent (showHeap t4), iNewline, iNewline
            , iStr "new frame ptr: "
            , iIndent (showFramePtr fp'), iNewline
            , iStr "^^^^^^^^^^^^^^^^^^^^^^^^", iNewline
            ]
  where f0 = heapBefore gcinfo
        (f1, t1) = heapEvacuatedByStack gcinfo
        (f2, t2) = heapEvacuatedByDump gcinfo
        (f3, t3) = heapEvacuatedByFramePtr gcinfo
        t4 = heapScavenged gcinfo
        fp' = fptrDone gcinfo


-- | NOTE: Closure = ([Instruction], FramePtr) なので
-- [Instruction] の中で使われる FramePtr はコピーし、それ以外は ([], FrameNull) で潰す
-- ただし [Instruction] で直接使われてなくても、Frame の relslots で間接的に参照している場合は保持し続ける
{- |
>>> let h = hInitial :: Heap Frame
>>> let cs = initCodeStore
>>> let (h1, a1) = hAlloc h (Frame [([Push (Arg 1)], FrameNull)][])
>>> let (h2, a2) = hAlloc h1 (Frame [([Push (Arg 2)], FrameAddr 1)] [])
>>> let ((from, to), fp) = evacuateFramePtr True cs h2 hInitial ([Push (Arg 1)], FrameAddr 2)
>>> from
(2,2,[(1,Forward 2),(2,Forward 1)])
>>> to
(2,2,[(1,Frame [([Push (Arg 2)],FrameAddr 1)] []),(2,Frame [([Push (Arg 1)],FrameNull)] [])])
>>> fp
FrameAddr 1

>>> let (h3, a3) = hAlloc h (Frame [([Push (Arg 1)], FrameNull),([Push (Arg 2)], FrameInt 42),([Push (Arg 3)], FrameAddr 2)] [])
>>> let (h4, a4) = hAlloc h3 (Frame [([Push (Arg 1)], FrameAddr 1)] [])
>>> let ((from2, to2), fp2) = evacuateFramePtr True cs h4 hInitial ([Push (Arg 1)], FrameAddr 2)
>>> from2
(2,2,[(1,Forward 2),(2,Forward 1)])
>>> to2
(2,2,[(1,Frame [([Push (Arg 1)],FrameAddr 1)] []),(2,Frame [([Push (Arg 1)],FrameNull),([Push (Arg 2)],FrameInt 42),([Push (Arg 3)],FrameAddr 2)] [])])
>>> fp2
FrameAddr 1

>>> let ((from3, to3), fp3) = evacuateFramePtr True cs h4 hInitial ([Push (Arg 2)], FrameAddr 2)
>>> from3
(2,2,[(2,Forward 1),(1,Frame [([Push (Arg 1)],FrameNull),([Push (Arg 2)],FrameInt 42),([Push (Arg 3)],FrameAddr 2)] [])])
>>> to3
(1,1,[(1,Frame [([],FrameNull)] [])])
>>> fp3
FrameAddr 1

>>> let ((from4, to4), fp4) = evacuateFramePtr True cs h4 hInitial ([Push (Arg 1)], FrameAddr 1)
>>> from4
(2,2,[(1,Forward 1),(2,Frame [([Push (Arg 1)],FrameAddr 1)] [])])
>>> to4
(1,1,[(1,Frame [([Push (Arg 1)],FrameNull),([],FrameNull),([],FrameNull)] [])])
>>> fp4
FrameAddr 1

>>> let ((from5, to5), fp5) = evacuateFramePtr True cs h4 hInitial ([Push (Arg 2)], FrameAddr 1)
>>> from5
(2,2,[(1,Forward 1),(2,Frame [([Push (Arg 1)],FrameAddr 1)] [])])
>>> to5
(1,1,[(1,Frame [([],FrameNull),([Push (Arg 2)],FrameInt 42),([],FrameNull)] [])])
>>> fp5
FrameAddr 1

>>> let ((from6, to6), fp6) = evacuateFramePtr True cs h4 hInitial ([Push (Arg 3)], FrameAddr 1)
>>> from6
(2,2,[(2,Forward 2),(1,Forward 1)])
>>> to6
(2,2,[(1,Frame [([],FrameNull),([],FrameNull),([Push (Arg 3)],FrameAddr 2)] []),(2,Frame [([Push (Arg 1)],FrameAddr 1)] [])])
>>> fp6
FrameAddr 1

>>> let ((from7, to7), fp7) = evacuateFramePtr True cs h4 hInitial ([Push (Arg 1), Enter (Arg 3)], FrameAddr 1)
>>> from7
(2,2,[(2,Forward 2),(1,Forward 1)])
>>> to7
(2,2,[(1,Frame [([Push (Arg 1)],FrameNull),([],FrameNull),([Push (Arg 3)],FrameAddr 2)] []),(2,Frame [([Push (Arg 1)],FrameAddr 1)] [])])
>>> fp7
FrameAddr 1
-}
{- | refer to the related used slots
>>> let h = hInitial
>>> let cs = initCodeStore
>>> let (h1, a1) = hAlloc h (Frame [([Push (Arg 1)], FrameNull),([Push (Arg 2)], FrameAddr 1),([Enter (Arg 1)], FrameAddr 1)] [(3,[1])])
>>> let ((from, to), fp) = evacuateFramePtr True cs h1 hInitial ([Push (Arg 3)], FrameAddr 1)
>>> from
(1,1,[(1,Forward 1)])
>>> to
(1,1,[(1,Frame [([Push (Arg 1)],FrameNull),([],FrameNull),([Enter (Arg 1)],FrameAddr 1)] [(3,[1])])])
-}
{- | refer to the related used slots 2-steps
>>> let h = hInitial
>>> let cs = initCodeStore
>>> let (h1, a1) = hAlloc h (Frame [([Push (Arg 1)], FrameNull),([Push (Arg 2)], FrameAddr 1),([Enter (Arg 1)], FrameAddr 1)] [(3,[1]),(1,[2])])
>>> let ((from, to), fp) = evacuateFramePtr True cs h1 hInitial ([Push (Arg 3)], FrameAddr 1)
>>> from
(1,1,[(1,Forward 1)])
>>> to
(1,1,[(1,Frame [([Push (Arg 1)],FrameNull),([Push (Arg 2)],FrameAddr 1),([Enter (Arg 1)],FrameAddr 1)] [(3,[1]),(1,[2])])])
-}
{- | refer to the related used slots muturally
>>> let h = hInitial
>>> let cs = initCodeStore
>>> let (h1, a1) = hAlloc h (Frame [([Push (Arg 1)], FrameNull),([Push (Arg 2)], FrameAddr 1),([Enter (Arg 1)], FrameAddr 1)] [(3,[1]),(1,[2]),(2,[3])])
>>> let ((from, to), fp) = evacuateFramePtr True cs h1 hInitial ([Push (Arg 3)], FrameAddr 1)
>>> from
(1,1,[(1,Forward 1)])
>>> to
(1,1,[(1,Frame [([Push (Arg 1)],FrameNull),([Push (Arg 2)],FrameAddr 1),([Enter (Arg 1)],FrameAddr 1)] [(3,[1]),(1,[2]),(2,[3])])])
-}
{- | The case for Cyclic Reference
>>> let h = hInitial :: Heap Frame
>>> let cs = initCodeStore
>>> let (h1, a1) = hAlloc h (Frame [([Push (Arg 1)], FrameAddr 2)] [])
>>> let (h2, a2) = hAlloc h1 (Frame [([Push (Arg 1)], FrameAddr 1)] [])
>>> let ((from, to), fp) = evacuateFramePtr True cs h2 hInitial ([Push (Arg 1)], FrameAddr 2)
>>> from
(2,2,[(1,Forward 2),(2,Forward 1)])
>>> to
(2,2,[(1,Frame [([Push (Arg 1)],FrameAddr 1)] []),(2,Frame [([Push (Arg 1)],FrameAddr 2)] [])])
>>> fp
FrameAddr 1

>>> let (h3, a3) = hAlloc h2 (Frame [([Push (Arg 1)], FrameAddr 2)] [])
>>> h3
(3,3,[(3,Frame [([Push (Arg 1)],FrameAddr 2)] []),(2,Frame [([Push (Arg 1)],FrameAddr 1)] []),(1,Frame [([Push (Arg 1)],FrameAddr 2)] [])])
>>> let ((from', to'), fp') = evacuateFramePtr True cs h3 hInitial ([Push (Arg 1)], FrameAddr 3)
>>> from'
(3,3,[(1,Forward 3),(2,Forward 2),(3,Forward 1)])
>>> to'
(3,3,[(1,Frame [([Push (Arg 1)],FrameAddr 2)] []),(2,Frame [([Push (Arg 1)],FrameAddr 1)] []),(3,Frame [([Push (Arg 1)],FrameAddr 2)] [])])

>>> let ((from2, to2), fp2) = evacuateFramePtr True cs h3 hInitial ([Push (Arg 1)], FrameAddr 2)
>>> from2
(3,3,[(1,Forward 2),(2,Forward 1),(3,Frame [([Push (Arg 1)],FrameAddr 2)] [])])
>>> to2
(2,2,[(1,Frame [([Push (Arg 1)],FrameAddr 1)] []),(2,Frame [([Push (Arg 1)],FrameAddr 2)] [])])
-}
{- | The case for Self-Cyclic Reference
>>> let h = hInitial :: Heap Frame
>>> let cs = initCodeStore
>>> let (h1, a1) = hAlloc h (Frame [([Push (Arg 1)], FrameAddr 2)] [])
>>> let (h2, a2) = hAlloc h1 (Frame [([Push (Arg 1)], FrameAddr 2)] []) -- self-cyclic reference
>>> let ((from, to), fp) = evacuateFramePtr True cs h2 hInitial ([Push (Arg 1)], FrameAddr 1)
>>> from
(2,2,[(2,Forward 2),(1,Forward 1)])
>>> to
(2,2,[(1,Frame [([Push (Arg 1)],FrameAddr 2)] []),(2,Frame [([Push (Arg 1)],FrameAddr 2)] [])])
>>> fp
FrameAddr 1

>>> let ((from', to'), fp') = evacuateFramePtr True cs h2 hInitial ([Push (Arg 1)], FrameAddr 2)
>>> from'
(2,2,[(2,Forward 1),(1,Frame [([Push (Arg 1)],FrameAddr 2)] [])])
>>> to'
(1,1,[(1,Frame [([Push (Arg 1)],FrameAddr 2)] [])])
>>> fp'
FrameAddr 1
-}
{- | check CodeStore
>>> let h = hInitial :: Heap Frame
>>> let cs = [("f", Compiled [1,3] [Push (Arg 1), Enter (Arg 3)])]
>>> let (h1, a1) = hAlloc h (Frame [([Take 1 1], FrameNull),([Take 2 2],FrameNull),([Take 3 3],FrameNull)] [])
>>> let (h2, a2) = hAlloc h1 (Frame [([Enter (Label "f")], FrameAddr 1)] [])
>>> let ((from, to), fp) = evacuateFramePtr True cs h2 hInitial ([Push (Arg 1)], FrameAddr 2)
>>> from
(2,2,[(1,Forward 2),(2,Forward 1)])
>>> to
(2,2,[(1,Frame [([Enter (Label "f")],FrameAddr 1)] []),(2,Frame [([Take 1 1],FrameNull),([Take 2 2],FrameNull),([Take 3 3],FrameNull)] [])])
>>> fp
FrameAddr 1
-}
{- | check Code
>>> let h = hInitial :: Heap Frame
>>> let (h1, a1) = hAlloc h (Frame [([Take 1 1], FrameNull),([Take 2 2],FrameNull),([Take 3 3],FrameNull)] [])
>>> let (h2, a2) = hAlloc h1 (Frame [([Enter (Code (Compiled [1,3] [Push (Arg 1), Enter (Arg 3)]))], FrameAddr 1)] [])
>>> let ((from, to), fp) = evacuateFramePtr True initCodeStore h2 hInitial ([Push (Arg 1)], FrameAddr 2)
>>> from
(2,2,[(1,Forward 2),(2,Forward 1)])
>>> to
(2,2,[(1,Frame [([Enter (Code (Compiled {slotsOf = [1,3], instrsOf = [Push (Arg 1),Enter (Arg 3)]}))],FrameAddr 1)] []),(2,Frame [([Take 1 1],FrameNull),([Take 2 2],FrameNull),([Take 3 3],FrameNull)] [])])
>>> fp
FrameAddr 1
-}
evacuateFramePtr :: Bool -> CodeStore -> TimHeap -> TimHeap -> ([Instruction], FramePtr) -> ((TimHeap, TimHeap), FramePtr)
evacuateFramePtr liveCheck cstore from to (instrs, fptr) = case fptr of
  FrameAddr a -> case hLookup from a of
    Frame clss rslots -> case hAlloc to (Frame [] []) of
      (to1, a') -> case hUpdate from a (Forward a') of
        from1 -> case mapAccumL (update rslots) (from1, to1) (zip [1..] clss) of
          ((from2, to2), clss') -> case hUpdate to2 a' (Frame clss' rslots) of -- TODO: rslots も削減可
            to3 -> ((from2, to3), FrameAddr a')

    -- すでに置き換え済
    Forward a'  -> ((from, to), FrameAddr a')
    where
      update :: RelSlot -> (TimHeap, TimHeap) -> (Int, Closure) -> ((TimHeap, TimHeap), Closure)
      update dict (f, t) (i, cls)
        | not liveCheck || i `elem` go liveArgs = (hs, cls)
        | otherwise                             = ((f, t), ([], FrameNull))
        where
          (hs, _) = evacuateFramePtr False cstore f t cls
          -- NOTE: ここで2段階以上の間接参照があるとスロットが GC されてしまう可能性がある
          -- 例えば [4 ~ [1,2], 2 ~ [3]] という状態で 4 が必要なら 3 も必要になる
          -- 循環参照もありえるので効率が悪いが停止条件は愚直にやっている
          go cur = if cur == new then cur else go new
            where new = nub . sort $ concatMap extract cur
          extract n = maybe [n] (n:) $ lookup n dict
      liveArgs :: [Int]
      liveArgs = nub . sort $ foldl' g [] instrs
        where
          g ns (Move _ am) = h ns am
          g ns (Push am)   = h ns am
          g ns (Enter am)  = h ns am
          g ns (Cond t f)  = nub $ foldl' g ns (t ++ f)
          g ns _           = ns

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
>>> let (h1, a1) = hAlloc h (Frame [([Push (Arg 1)], FrameNull)] [])
>>> let (h2, a2) = hAlloc h1 (Frame [([Push (Arg 2)], FrameAddr 1)] [])
>>> let ((from, to), stk) = evacuateStack cs h2 hInitial [([Push (Arg 1)], FrameAddr 2), ([Push (Arg 2)], FrameAddr 1)]
>>> from
(2,2,[(1,Forward 2),(2,Forward 1)])
>>> to
(2,2,[(1,Frame [([Push (Arg 2)],FrameAddr 1)] []),(2,Frame [([Push (Arg 1)],FrameNull)] [])])
>>> stk
[([Push (Arg 1)],FrameAddr 1),([Push (Arg 2)],FrameAddr 2)]
-}
evacuateStack :: CodeStore -> TimHeap -> TimHeap -> TimStack -> ((TimHeap, TimHeap), TimStack)
evacuateStack cstore from to stk = case mapAccumL update (from, to) stk of
  (hs, fps) -> (hs, zipWith (\(is, _) fp -> (is, fp)) stk fps)
  where
    update :: (TimHeap, TimHeap) -> Closure -> ((TimHeap, TimHeap), FramePtr)
    update (f, t) = evacuateFramePtr False cstore f t

-- | NOTE: Dump はまだ使われてないので id 的な実装になっている
evacuateDump :: CodeStore -> TimHeap -> TimHeap -> TimDump -> ((TimHeap, TimHeap), TimDump)
evacuateDump cstore from to dmp = ((from, to), dmp)

-- | 新しいヒープ中の FramePtr を 古いヒープから探して、
--   新しいヒープのどのアドレスに Forward されているか見て付け替えていく
{- |
>>> let from = (2,2,[3..],[(1,Forward 2),(2,Forward 1)])
>>> let to = (2,2,[3..],[(1,Frame [([Push (Arg 2)],FrameAddr 1)] []),(2,Frame [([Push (Arg 1)],FrameNull)] [])])
>>> scavenge from to
(2,2,[(2,Frame [([Push (Arg 1)],FrameNull)] []),(1,Frame [([Push (Arg 2)],FrameAddr 2)] [])])
-}
scavenge :: TimHeap -> TimHeap -> TimHeap
scavenge from to@(_, _, _, hp) = foldl' phi to hp
  where
    phi :: TimHeap -> (Addr, Frame) -> TimHeap
    phi t (a', f) = case f of
      Frame cls rslots -> hUpdate t a' (Frame (map conv cls) rslots)
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
                    , dump         = dmp
                    , heap         = hp
                    , codes        = cstore
                    }
  = case instrs of
  (Take t n:instr)
    | length stk >= n
      -- NOTE: Take は exec time にカウントしない (exercise 4.2)
      -> applyToStats (statIncHeapAllocated $ t + 1)
         (putInstructions instr
          . putFrame fptr'
          . putStack stk'
          . putHeap hp'
          $ state)
    | otherwise -> error "Too few args for Take instruction"
    where
      (hp', fptr') = fAlloc hp (Frame (take n stk ++ replicate (t-n) ([], FrameNull)) [])
      stk' = drop n stk
  (Move n am:istr) -> applyToStats statIncExecTime
                      (putInstructions istr
                       . putHeap hp2
                       $ state)
    where
      -- NOTE: Code 以外も処理されてしまうがコンパイラがバグってなければ問題ないはず
      hp1 = fUpdate hp fptr n (amToClosure am fptr hp cstore)
      hp2 = case am of
        Code cs -> fSetRelSlots hp1 fptr (n, slotsOf cs)
        _       -> hp1

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
  (PushMarker x:istr)
    -> applyToStats statIncExecTime
       (putInstructions istr
         . putStack []
         . putDump ((fptr, x, stk):dmp)
         $ state)
  [Return] -> case stk of
    [] -> applyToStats statIncExecTime
          (putStack stk'
           . putDump dmp'
           . putHeap hp'
           $ state)
      where
        ((fu, x, stk'), dmp') = case dmp of
          []  -> error "Return applied to empty dump"
          d:ds -> (d, ds)
        n = case vstk of
          (n:ns) -> n
          _      -> error "Return applied to empty vstk"
        hp' = fUpdate hp fu x (intCode, FrameInt n)
    _ -> applyToStats statIncExecTime
                   (putInstructions instr'
                    . putFrame fptr'
                    . putStack stk'
                    $ state)
      where
        ((instr', fptr'), stk') = case stk of
          []       -> error "Return applied to empty stack"
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
showSCDefns TimState { codes = cstore } = iInterleave iNewline (map showSC cstore)

showSC :: (Name, CompiledCode) -> IseqRep
showSC (name, cs)
  = iConcat [ iStr "Code for ", iStr name, iStr ":", iNewline
            , iStr "   "
            , iIndent (iConcat [ iStr "  used slots: ", showUsedSlots ns, iNewline
                               , iStr "instructions: ", showInstructions Full instrs, iNewline
                               ])
            ]
    where Compiled ns instrs = cs

showState :: TimState -> IseqRep
showState TimState { instructions = is
                   , frame        = fptr
                   , stack        = stk
                   , valstack     = vstk
                   , dump         = dmp
                   , heap         = hp
                   , codes        = cs
                   , stats        = stat
                   }
  = iConcat $ [ iStr "Code:  "
              , showInstructions Terse is, iNewline
              , iStr "Frame: "
              , showFrame hp fptr, iNewline
              , iStr "Rel slots: "
              , showRelSlots hp fptr, iNewline
              , iStr "Arg stack: "
              , showStack stk, iNewline
              , iStr "Value stack: "
              , showValueStack vstk, iNewline
              , iStr "Dump: "
              , showDump dmp, iNewline
              ] ++ gcinfo
    where gcinfo = case find (\i -> stepAt i == getSteps stat) (getGCInfo stat) of
            Nothing -> []
            Just i  -> [ iStr "GC: "
                       , iIndent (showGCInfo i), iNewline
                       ]

showUsedSlots :: UsedSlots -> IseqRep
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
showInstruction d (Take t n)     = iConcat [iStr "Take ", iNum t, iStr " ", iNum n]
showInstruction d (Move n a)     = iConcat [iStr "Move ", iNum n, iStr " ", showArg d a]
showInstruction d (Enter x)      = iStr "Enter " `iAppend` showArg d x
showInstruction d (Push x)       = iStr "Push " `iAppend` showArg d x
showInstruction d (PushV x)      = iStr "PushV " `iAppend` showValueAMode x
showInstruction d (PushMarker x) = iStr "PushMarker " `iAppend` iNum x
showInstruction d Return         = iStr "Return"
showInstruction d (Op op)        = iStr "Op " `iAppend` iStr (show op)
showInstruction d (Cond t f)     = iConcat [ iStr "Cond "
                                           , showInstructions d t
                                           , iStr " "
                                           , showInstructions d f
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
showFrame heap FrameNull = iStr "(ptr) null"
showFrame heap (FrameAddr addr)
  = iConcat [ iStr "#", iNum addr, iStr ": "
            , iStr "<"
            , iIndent (either showAddr (iInterleave iNewline . map showClosure) (fList frm))
            , iStr ">"
            ]
  where
    frm = hLookup heap addr
    showAddr :: Addr -> IseqRep
    showAddr a = iStr "FW #->" `iAppend` iNum a
showFrame heap (FrameInt n) = iConcat [ iStr "(int) ", iNum n ]

showRelSlots :: TimHeap -> FramePtr -> IseqRep
showRelSlots heap (FrameAddr addr)
  = iConcat [ iStr "["
            , iInterleave (iStr ", ") (map showRelSlot (fRelSlots frm))
            , iStr "]"
            ]
  where
    frm = hLookup heap addr
    showRelSlot :: (UsedSlot, UsedSlots) -> IseqRep
    showRelSlot (n, ns) = iConcat [ iNum n, iStr " ~ ", showUsedSlots ns ]
showRelSlots heap _ = iNil


showHeap :: TimHeap -> IseqRep
showHeap heap@(_, _, _, hp)
  = iConcat [ iStr "Heap: ["
            , iIndent (iInterleave iNewline $ map showHeapItem hp)
            , iStr "]"
            ]
  where
    showHeapItem :: (Addr, Frame) -> IseqRep
    showHeapItem (addr, fr)
      = iConcat [ iStr " "
                , showFrame heap (FrameAddr addr)
                ]

showFWAddr :: Addr -> IseqRep
showFWAddr addr = iStr (space (4 - length str) ++ str)
  where str = show addr

showStack :: TimStack -> IseqRep
showStack stk
  = iConcat [ iStr "["
            , iIndent (iInterleave iNewline (map showClosure stk))
            , iStr "]"
            ]

showValueStack :: TimValueStack -> IseqRep
showValueStack vstack = iConcat [ iStr "["
                                , iIndent (iInterleave (iStr ",") (map iNum vstack))
                                , iStr "]"
                                ]

showVStackTopValue :: TimValueStack -> IseqRep
showVStackTopValue []   = error "empty value stack"
showVStackTopValue [v]  = iNum v
showVStackTopValue vstk = error $ "value stack has more than 1 value: " ++ show vstk

showDump :: TimDump -> IseqRep
showDump dump
  = iConcat [ iStr "["
            , iIndent (iInterleave iNewline (map showTriple dump))
            , iStr "]"
            ]
  where showTriple (fp, idx, stk)
          = iConcat [ iStr "("
                    , showFramePtr fp
                    , iStr ", "
                    , iNum idx
                    -- , iStr ", "
                    -- , showStack stk
                    , iStr ")"
                    ]


showClosure :: Closure -> IseqRep
showClosure (i, f)
  = iConcat [ iStr "("
            , showInstructions Terse i
            , iStr ", "
            , showFramePtr f
            , iStr ")"
            ]

showFramePtr :: FramePtr -> IseqRep
showFramePtr FrameNull     = iStr "null"
showFramePtr (FrameAddr a) = iStr "#" `iAppend` iNum a
showFramePtr (FrameInt n)  = iStr "int " `iAppend` iNum n

showGCInfoSimple :: [GCInfo] -> IseqRep
showGCInfoSimple xs | null xs   = iConcat [ iNum 0, iNewline ]
                    | otherwise = iConcat [ iNum (length xs)
                                          , iStr " { "
                                          , iIndent (iInterleave iNewline $ map showResize xs)
                                          , iStr " }"
                                          ]
  where showResize (GCInfo { stepAt = n, heapBefore = f, heapScavenged = t})
          = iConcat [ iFWNum nod (n+1), iStr ") ", iNum (hSize f), iStr " -> ", iNum (hSize t) ]
        -- max number of steps
        s = stepAt $ last xs
        -- number of digits in the max number of steps
        nod = floor (logBase 10 (fromIntegral s)) + 1

showStats :: TimState -> IseqRep
showStats TimState { stats = st }
  = iConcat [ iStr "    Steps taken = ", iNum (statGetSteps st), iNewline
            , iStr "      Exec time = ", iNum (statGetExecTime st), iNewline
            , iStr " Heap allocated = ", iNum (statGetHeapAllocated st), iNewline
            , iStr "Max stack depth = ", iNum (statGetMaxStackDepth st), iNewline
            , iStr "        GC call = ", showGCInfoSimple (statGetGCInfo st), iNewline
            ]

data HowMuchToPrint = None
                    | Terse
                    | Full
                    deriving (Eq, Show)
