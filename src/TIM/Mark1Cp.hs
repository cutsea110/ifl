{-# LANGUAGE FlexibleInstances #-}
module TIM.Mark1Cp
  ( parse
  , compile
  , eval
  , showResults
  , runProg
  ) where

import Data.List (mapAccumL, nub)
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
  where showR | verbose   = showFullResults
              | otherwise = showResults

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

data TimValueStack = DummyTimValueStack
                   deriving (Eq, Show)
data TimDump = DummyTimDump
             deriving (Eq, Show)
type TimHeap = Heap Frame
instance {-# Overlapping #-} Show (Heap Frame) where
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

type CodeStore = Assoc Name [Instruction]

codeLookup :: CodeStore -> Name -> [Instruction]
codeLookup cstore l
  = aLookup cstore l $ error $ "Attempt to jump to unknown label " ++ show l

data TimStats
  = TimStats { getSteps         :: Int  -- The number of steps
             , getExecTime      :: Int  -- The execution time
             , getHeapAllocated :: Int  -- The amount of heap allocated
             , getMaxStackDepth :: Int  -- The maximum stack depth
             , getGCCount       :: Int  -- The count of garbage collections
             }
  deriving (Eq, Show)

statInitial :: TimStats
statInitial = TimStats { getSteps = 0
                       , getExecTime = 0
                       , getHeapAllocated = 0
                       , getMaxStackDepth = 0
                       , getGCCount = 0
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

statGetGCCount :: TimStats -> Int
statGetGCCount s = getGCCount s

statIncGCCount :: TimStats -> TimStats
statIncGCCount sts = sts { getGCCount = getGCCount sts + 1 }

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
  | null args = (name, instructions)
  | otherwise = (name, Take (length args) : instructions)
  where
    instructions = compileR body new_env
    new_env = zip args (map Arg [1..]) ++ env

compileR :: CoreExpr -> TimCompilerEnv -> [Instruction]
compileR (EAp e1 e2) env = Push (compileA e2 env) : compileR e1 env
compileR (EVar v) env = [Enter (compileA (EVar v) env)]
compileR (ENum n) env = [Enter (compileA (ENum n) env)]
compileR e        env = error $ "compileR: can't do this yet: " ++ show e

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
  | needGC    = applyToStats statIncGCCount $ gc state'
  | otherwise = state'
  where
    needGC = hSize (getHeap state) >= threshold
    state' = applyToStats statIncSteps state

gc :: TimState -> TimState
gc state@TimState { instructions = instrs
                  , frame        = fptr
                  , stack        = stk
                  , dump         = dmp
                  , heap         = from
                  }
  = case evacuateFramePtr from hInitial (instrs, fptr) of
  ((from1, to1), fptr1) -> case evacuateStack from1 to1 stk of
    ((from2, to2), stk1) -> case evacuateDump from2 to2 dmp of
      ((from3, to3), dmp1) -> case scavenge from3 to3 of
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
      , iStr "instr: ", iNewline
      , showInstructions Full is, iNewline
      , iStr "frame ptr: ", showFramePtr fp, iNewline
      , iStr "before", iNewline
      , showHeap f0, iNewline
      , iStr "evacuated frameptr: from1", iNewline
      , showHeap f1, iNewline
      , iStr "evacuated frameptr: to1", iNewline
      , showHeap t1, iNewline
      , iStr "evacuated stack: from2", iNewline
      , showHeap f2, iNewline
      , iStr "evacuated stack: to2", iNewline
      , showHeap t2, iNewline
      , iStr "evacuated dump: from3", iNewline
      , showHeap f3, iNewline
      , iStr "evacuated dump: to3", iNewline
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
>>> let (h1, a1) = hAlloc h (Frame [([Push (Arg 1)], FrameNull)])
>>> let (h2, a2) = hAlloc h1 (Frame [([Push (Arg 2)], FrameAddr 1)])
>>> let ((from, to), fp) = evacuateFramePtr h2 hInitial ([Push (Arg 1)], FrameAddr 2)
>>> from
(2,2,[(1,Forward 2),(2,Forward 1)])
>>> to
(2,2,[(1,Frame [([Push (Arg 2)],FrameAddr 1)]),(2,Frame [([],FrameNull)])])
>>> fp
FrameAddr 1

>>> let (h3, a3) = hAlloc h (Frame [([Push (Arg 1)], FrameNull),([Push (Arg 2)], FrameInt 42),([Push (Arg 3)], FrameAddr 2)])
>>> let (h4, a4) = hAlloc h3 (Frame [([Push (Arg 1)], FrameAddr 1)])
>>> let ((from2, to2), fp2) = evacuateFramePtr h4 hInitial ([Push (Arg 1)], FrameAddr 2)
>>> from2
(2,2,[(1,Forward 2),(2,Forward 1)])
>>> to2
(2,2,[(1,Frame [([Push (Arg 1)],FrameAddr 1)]),(2,Frame [([Push (Arg 1)],FrameNull),([],FrameNull),([],FrameNull)])])
>>> fp2
FrameAddr 1

>>> let ((from3, to3), fp3) = evacuateFramePtr h4 hInitial ([Push (Arg 2)], FrameAddr 2)
>>> from3
(2,2,[(2,Forward 1),(1,Frame [([Push (Arg 1)],FrameNull),([Push (Arg 2)],FrameInt 42),([Push (Arg 3)],FrameAddr 2)])])
>>> to3
(1,1,[(1,Frame [([],FrameNull)])])
>>> fp3
FrameAddr 1

>>> let ((from4, to4), fp4) = evacuateFramePtr h4 hInitial ([Push (Arg 1)], FrameAddr 1)
>>> from4
(2,2,[(1,Forward 1),(2,Frame [([Push (Arg 1)],FrameAddr 1)])])
>>> to4
(1,1,[(1,Frame [([Push (Arg 1)],FrameNull),([],FrameNull),([],FrameNull)])])
>>> fp4
FrameAddr 1

>>> let ((from5, to5), fp5) = evacuateFramePtr h4 hInitial ([Push (Arg 2)], FrameAddr 1)
>>> from5
(2,2,[(1,Forward 1),(2,Frame [([Push (Arg 1)],FrameAddr 1)])])
>>> to5
(1,1,[(1,Frame [([],FrameNull),([Push (Arg 2)],FrameInt 42),([],FrameNull)])])
>>> fp5
FrameAddr 1

>>> let ((from6, to6), fp6) = evacuateFramePtr h4 hInitial ([Push (Arg 3)], FrameAddr 1)
>>> from6
(2,2,[(2,Forward 2),(1,Forward 1)])
>>> to6
(2,2,[(1,Frame [([],FrameNull),([],FrameNull),([Push (Arg 3)],FrameAddr 2)]),(2,Frame [([],FrameNull)])])
>>> fp6
FrameAddr 1

>>> let ((from7, to7), fp7) = evacuateFramePtr h4 hInitial ([Push (Arg 1), Enter (Arg 3)], FrameAddr 1)
>>> from7
(2,2,[(2,Forward 2),(1,Forward 1)])
>>> to7
(2,2,[(1,Frame [([Push (Arg 1)],FrameNull),([],FrameNull),([Push (Arg 3)],FrameAddr 2)]),(2,Frame [([],FrameNull)])])
>>> fp7
FrameAddr 1
-}
evacuateFramePtr :: TimHeap -> TimHeap -> ([Instruction], FramePtr) -> ((TimHeap, TimHeap), FramePtr)
evacuateFramePtr from to (instrs, fptr) = case fptr of
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
        | i `elem` liveArgs = case evacuateFramePtr f t cls of
            (hs, _) -> (hs, (is, fp)) -- NOTE: ここで fp' としないで scavenge がやる
        | otherwise         = ((f, t), ([], FrameNull))
      liveArgs :: [Int]
      liveArgs = nub $ foldl g [] instrs
        where
          g ns (Push (Arg n))  = n : ns
          g ns (Enter (Arg n)) = n : ns
          g ns _               = ns
              
  -- Heap には含まないので from と to で変わらない
  FrameInt n -> ((from, to), fptr)
  FrameNull  -> ((from, to), fptr)

{- |
>>> let h = hInitial :: Heap Frame
>>> let (h1, a1) = hAlloc h (Frame [([Push (Arg 1)], FrameNull)])
>>> let (h2, a2) = hAlloc h1 (Frame [([Push (Arg 2)], FrameAddr 1)])
>>> let ((from, to), stk) = evacuateStack h2 hInitial [([Push (Arg 1)], FrameAddr 2), ([Push (Arg 2)], FrameAddr 1)]
>>> from
(2,2,[(1,Forward 2),(2,Forward 1)])
>>> to
(2,2,[(1,Frame [([Push (Arg 2)],FrameAddr 1)]),(2,Frame [([],FrameNull)])])
>>> stk
[([Push (Arg 1)],FrameAddr 1),([Push (Arg 2)],FrameAddr 2)]
-}
evacuateStack :: TimHeap -> TimHeap -> TimStack -> ((TimHeap, TimHeap), TimStack)
evacuateStack from to stk = case mapAccumL update (from, to) stk of
  (hs,fps) -> (hs, zipWith (\(is, _) fp -> (is, fp)) stk fps)
  where
    update :: (TimHeap, TimHeap) -> ([Instruction], FramePtr) -> ((TimHeap, TimHeap), FramePtr)
    update (f, t) (is, fp) = evacuateFramePtr f t (is, fp)

-- | NOTE: Dump はまだ使われてないので id 的な実装になっている
evacuateDump :: TimHeap -> TimHeap -> TimDump -> ((TimHeap, TimHeap), TimDump)
evacuateDump from to dump = ((from, to), dump)

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
                    , heap         = hp
                    , codes        = cstore
                    }
  = case instrs of
  (Take n:instr)
    | length stk >= n
      -- Take は exec time にカウントしない (exercise 4.2)
      -> applyToStats (statIncHeapAllocated $ n + 1)
         (putInstructions instr
          . putFrame fptr'
          . putStack stk'
          . putHeap hp'
          $ state)
    | otherwise       -> error "Too few args for Take instruction"
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
  _          -> error $ "invalid instructions: " ++ show instrs


amToClosure :: TimAMode -> FramePtr -> TimHeap -> CodeStore -> Closure
amToClosure (Arg n)   fptr heap cstore = fGet heap fptr n
amToClosure (Code il) fptr heap cstore = (il, fptr)
amToClosure (Label l) fptr heap cstore = (codeLookup cstore l, fptr)
amToClosure (IntConst n) fptr heap cstore = (intCode, FrameInt n)

intCode :: [Instruction]
intCode = []

showFullResults :: [TimState] -> String
showFullResults states
  = unlines (map iDisplay
             ([ iStr "Supercombinator definitions", iNewline, iNewline
              , showSCDefns frist_state, iNewline, iNewline
              , iStr "State transitions", iNewline
              ] ++
              iLayn' (map showState states) ++
              [ showStats (last states)
              ])
            )
  where (frist_state:rest_states) = states

showSCDefns :: TimState -> IseqRep
showSCDefns state@TimState { codes = cstore }
  = iInterleave iNewline (map showSC cstore)

showSC :: (Name, [Instruction]) -> IseqRep
showSC (name, instrs)
  = iConcat [ iStr "Code for ", iStr name, iStr ":", iNewline
            , iStr "   ", showInstructions Full instrs, iNewline, iNewline
            ]

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

showInstructions :: HowMuchToPrint -> [Instruction] -> IseqRep
showInstructions None il = iStr "{..}"
showInstructions Terse il
  = iConcat [ iStr "{", iIndent (iInterleave (iStr ", ") body), iStr "}"]
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
showInstruction d (Take n)  = iStr "Take " `iAppend` iNum n
showInstruction d (Enter x) = iStr "Enter " `iAppend` showArg d x
showInstruction d (Push x)  = iStr "Push " `iAppend` showArg d x

showArg :: HowMuchToPrint -> TimAMode -> IseqRep
showArg d (Arg n)   = iStr "Arg " `iAppend` iNum n
showArg d (Code il) = iStr "Code " `iAppend` showInstructions d il
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
showValueStack vstack = iNil

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

showStats :: TimState -> IseqRep
showStats state@TimState { stats = stats }
  = iConcat [ iStr "Total number of steps = ", iNum (statGetSteps stats), iNewline
            , iStr "            Exec time = ", iNum (statGetExecTime stats), iNewline
            , iStr "       Heap allocated = ", iNum (statGetHeapAllocated stats), iNewline
            , iStr "      Max stack depth = ", iNum (statGetMaxStackDepth stats), iNewline
            , iStr "              GC call = ", iNum (statGetGCCount stats), iNewline
            ]

showResults :: [TimState] -> String
showResults states
  = iDisplay $ iConcat [ showState last_state, iNewline
                       , showStats last_state
                       ]
  where last_state = last states

fullRun :: String -> String
fullRun = showFullResults . eval . compile . parse

data HowMuchToPrint = None
                    | Terse
                    | Full
                    deriving (Eq, Show)
