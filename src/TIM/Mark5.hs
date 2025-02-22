{-# LANGUAGE FlexibleInstances #-}
module TIM.Mark5
  ( parse
  , compile
  , eval
  , showResults
  , runProg
  , Config(..)
  ) where

import Control.Arrow (second)
import Data.List (find, foldl', intersect, mapAccumL, nub, sort)

import Heap
import Iseq
import Language
import Utils

data Config = Config { verbose           :: !Bool
                     , gcThreshold       :: !Int
                     , convertToListBase :: !Bool
                     }

runProg :: Config -> String -> String
runProg conf
  | convertToListBase conf = showR . eval conf . cnv . compile . parse
  | otherwise              = showR . eval conf . compile . parse
  where showR | verbose conf = showResults
              | otherwise    = showSimpleResult

-- | convert to newer version
--   main := cons main nil
cnv :: TimState -> TimState
cnv state = state { instructions = [Enter (Label "__main")] }


data Instruction = Take Int Int       -- take t n
                 | Move Int TimAMode
                 | Enter TimAMode
                 | Push TimAMode
                 | PushV ValueAMode
                 | PushMarker Int
                 | UpdateMarkers Int
                 | Return
                 | ReturnConstr Tag
                 | Op Op
                 | Switch [Branch]
                 | Print
                 deriving (Eq, Show)

type Branch = (Tag, [Instruction])
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
              | Data Int
              | Label Name
              | Code CompiledCode
              | IntConst Int
              deriving (Eq, Show)

data TimState = TimState { instructions :: [Instruction]
                         , frame        :: FramePtr
                         , data_frame   :: FramePtr
                         , stack        :: TimStack
                         , valstack     :: TimValueStack
                         , dump         :: TimDump
                         , heap         :: TimHeap
                         , codes        :: CodeStore
                         , output       :: TimOutput
                         , stats        :: TimStats
                         }
              deriving (Show)

getInstructions :: TimState -> [Instruction]
getInstructions = instructions
putInstructions :: [Instruction] -> TimState -> TimState
putInstructions instrs state = state { instructions = instrs }
getFrame :: TimState -> FramePtr
getFrame = frame
putFrame :: FramePtr -> TimState -> TimState
putFrame fr state = state { frame = fr }
getDataFrame :: TimState -> FramePtr
getDataFrame = data_frame
putDataFrame :: FramePtr -> TimState -> TimState
putDataFrame dfr state = state { data_frame = dfr }
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
getOutput :: TimState -> TimOutput
getOutput = output
putOutput :: TimOutput -> TimState -> TimState
putOutput o state = state { output = o }
outputLast :: TimOutput -> IseqRep
outputLast (_, i) = i
clearOutputLast :: TimState -> TimState
clearOutputLast state = putOutput (o, iNil) state
  where (o, _) = getOutput state


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

type TimOutput = ([Int], IseqRep)

data Frame = Frame [Closure] RelSlot
           | Forward Addr
           deriving (Eq, Show)

fAlloc :: TimHeap -> Frame -> (TimHeap, FramePtr)
fAlloc heap xs = (heap', FrameAddr addr)
  where
    (heap', addr) = hAlloc heap xs

fGet :: TimHeap -> FramePtr -> Int -> Closure
fGet heap (FrameAddr addr) n = case frm of
  Frame f _ -> f !! (n-1)
  Forward _ -> error $ "fGet: Unexpected " ++ show frm
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
      Frame cs m -> (cs, m)
      Forward _  -> error $ "fUpdate: Unexpected " ++ show frm
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

data GCInfo = GCInfo { stepAt                      :: Int
                     , instr                       :: [Instruction]
                     , stackInit                   :: TimStack
                     , fptrInit                    :: FramePtr
                     , dfptrInit                   :: FramePtr
                     , heapBefore                  :: TimHeap
                     , heapEvacuatedByStack        :: (TimHeap, TimHeap)
                     , heapEvacuatedByDump         :: (TimHeap, TimHeap)
                     , heapEvacuatedByFramePtr     :: (TimHeap, TimHeap)
                     , heapEvacuatedByDataFramePtr :: (TimHeap, TimHeap)
                     , heapScavenged               :: TimHeap
                     , fptrDone                    :: FramePtr
                     , dfptrDone                   :: FramePtr
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
             , data_frame   = FrameNull
             , stack        = [(top_cont_code, init_fp)]
             , valstack     = initialValueStack
             , dump         = initialDump
             , heap         = init_heap
             , codes        = compiled_code
             , output       = ([], iNil)
             , stats        = statInitial
             }
  where
    sc_defs = preludeDefs ++ extraPreludeDefs ++ program
    compiled_sc_defs = map (compileSc initial_env) sc_defs
    compiled_code = bootstraps ++ compiled_sc_defs ++ compiledPrimitives
    top_cont_code = instrsOf $ snd topCont
    (init_heap, init_fp)
      = fAlloc hInitial (Frame [([], FrameNull), ([], FrameNull)] []) -- topCont needs 2 slots frame
    initial_env = [(name, Label name) | (name, _, _) <- sc_defs] ++
                  [(name, Label name) | (name, _) <- compiledPrimitives]

bootstraps :: [(Name, CompiledCode)]
bootstraps = [topCont, headCont]

topCont :: (Name, CompiledCode)
topCont = ("__topCont"
          , Compiled [1,2] [ Switch [ (1, [])
                                    , (2, [ Move 1 (Data 1)  -- Head
                                          , Move 2 (Data 2)  -- Tail
                                          , Push (Label "__headCont")
                                          , Enter (Arg 1)
                                          ])
                                    ]
                           ]
          )

headCont :: (Name, CompiledCode)
headCont = ("__headCont", Compiled [1,2] [Print, Push (Label "__topCont"), Enter (Arg 2)])

initialValueStack :: TimValueStack
initialValueStack = []

initialDump :: TimDump
initialDump = []

initCodeStore :: CodeStore
initCodeStore = []

extraPreludeDefs :: CoreProgram
extraPreludeDefs = [ ("cons", [], EConstr 2 2)
                   , ("nil",  [], EConstr 1 0)
                   , ("true", [], EConstr 2 0)
                   , ("false",[], EConstr 1 0)
                   , ("if", ["c", "t", "f"], ECase (EVar "c") [(1, [], EVar "f"), (2, [], EVar "t")])
                   -- __main will use in case of convertList option is on
                   , ("__main", [], EAp (EAp (EVar "cons") (EVar "main")) (EVar "nil"))
                   ]

data OpType = BinOp Op | UniOp Op deriving (Eq, Show)

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
type TimCompilerEnv = [(Name, TimAMode)]

compileSc :: TimCompilerEnv -> CoreScDefn -> (Name, CompiledCode)
compileSc env (name, args, body)
  -- exercise 4.3
  | d == 0    = (name, cs) -- d == 0 means n == 0 too
  -- exercise 4.22
  | n == 0    = (name, Compiled ns (Take d 0:il))
  | otherwise = (name, Compiled ns (UpdateMarkers n:Take d n:il))
  where
    n = length args
    (d, cs@(Compiled ns il)) = compileR body new_env n
    new_env = zip args (map Arg [1..]) ++ env

compileR :: CoreExpr -> TimCompilerEnv -> OccupiedSlotIdx -> (OccupiedSlotIdx, CompiledCode)
compileR (EConstr t a) env d
  -- exercise 4.24
  | a == 0    = (d, Compiled [] [ReturnConstr t])
  | otherwise = (d, Compiled [] [UpdateMarkers a, Take a a, ReturnConstr t])
compileR (ECase e alts) env d = (d', Compiled (merge ns us') (Push (Code (Compiled us' [Switch brs])):ise))
    where
      (ds, us, brs) = unzip3 $ map (compileE env d) alts
      us' = nub . sort $ concat us
      (d', Compiled ns ise) = compileR e env (maximum ds)
compileR (ELet isrec defns body) env d = (d', Compiled (merge ns ns') (moves ++ il))
    where
      n = length defns
      frame_slots = [d+1..d+n]
      (dn, ams) = mapAccumL (\ix (u, (_, e')) -> compileU e' u env' ix) (d+n) $ zip frame_slots defns
      env'     | isrec     = let_env
               | otherwise = env
      let_env = zip (map fst defns) (map mkIndMode frame_slots) ++ env
      (d', Compiled ns il) = compileR body let_env dn
      moves = zipWith Move frame_slots ams
      ns' = nub . sort $ concatMap usedSlots ams -- moves で使われているスロット
compileR e@(EAp e1 e2) env d
  | isBasicOp e = compileB e env (d, Compiled [] [Return])
  -- exercise 4.7
  | isAtomic e2 = let am = compileA e2 env
                      (d2, Compiled ns2 il2) = compileR e1 env d
                      ns1 = usedSlots am
                  in (d2, Compiled (merge ns1 ns2) (Push am:il2))
  -- exercise 4.20
  | otherwise = let (d1, am) = compileU e2 (d+1) env (d+1)
                    (d2, Compiled ns2 il2) = compileR e1 env d1
                    ns1 = usedSlots am
                    is = Move (d+1) am:Push (Code (Compiled [d+1] [Enter (Arg (d+1))])):il2
                in (d2, Compiled (merge ns1 ns2) is)
compileR (EVar v) env d = (d, Compiled ns (mkEnter am))
    where am = compileA (EVar v) env
          ns = usedSlots am -- am で使われているスロット
compileR (ENum n) env d = (d, Compiled [] [PushV (IntVConst n), Return])
compileR e _ _ = error $ "compileR: not do this yet: " ++ show e

usedSlots :: TimAMode -> [OccupiedSlotIdx]
usedSlots (Arg i)   = [i]
usedSlots (Code cs) = slotsOf cs -- NOTE: EVar, ENum のときは今のところこれは起きないはず?
usedSlots _         = []

merge :: UsedSlots -> UsedSlots -> UsedSlots
merge a b = nub . sort $ a ++ b

-- | I scheme
mkIndMode :: Int -> TimAMode
mkIndMode n = Code (Compiled [n] [Enter (Arg n)])

-- | J scheme
mkUpdIndMode :: Int -> TimAMode
mkUpdIndMode n = Code (Compiled [n] [PushMarker n, Enter (Arg n)]) -- NOTE: mkEnter 不要

-- exercise 4.17
mkEnter :: TimAMode -> [Instruction]
mkEnter (Code am) = instrsOf am
mkEnter am        = [Enter am]

compileB :: CoreExpr -> TimCompilerEnv -> (OccupiedSlotIdx, CompiledCode) -> (OccupiedSlotIdx, CompiledCode)
compileB e env (d, Compiled slots cont)
  | isBinOp e = (max d1 d2, Compiled (merge slots1 slots2) il2)
  where (e1, op, e2) = unpackBinOp e
        (d1, am1@(Compiled slots1 _))  = compileB e1 env (d, Compiled slots (Op op : cont))
        (d2,      Compiled slots2 il2) = compileB e2 env (d, am1)
compileB e env (d, Compiled slots cont)
  | isUniOp e = compileB e1 env (d, Compiled slots (Op op : cont))
  where (op, e1) = unpackUniOp e
compileB (ENum n) env (d, Compiled slots cont) = (d, Compiled slots (PushV (IntVConst n) : cont))
compileB e env (d, cont@(Compiled slots _)) = (d', Compiled (merge slots slots') (Push (Code cont) : cont'))
  where (d', Compiled slots' cont') = compileR e env d

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

isAtomic :: CoreExpr -> Bool
isAtomic (EVar _) = True
isAtomic (ENum _) = True
isAtomic _        = False

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

compileE :: TimCompilerEnv -> OccupiedSlotIdx -> CoreAlt -> (OccupiedSlotIdx, UsedSlots, Branch)
compileE env d (tag, vars, body) = (d', merge ns used_slots, (tag, new_body))
  where
    no_of_args = length vars
    used_slots = [d+1..d+no_of_args]
    -- section 4.6.5 optimization idea-2
    is_moves = map (\i -> Move i (Data (i-d))) $ used_slots `intersect` ns
    env' = zipWith (\n i -> (n, Arg i)) vars used_slots ++ env
    (d', Compiled ns is_body) = compileR body env' (d+no_of_args)
    new_body = case is_moves ++ is_body of
      -- section 4.6.5 optimization idea-1
      [Move i (Data j), Enter (Arg k)] | i == k -> [Enter (Data j)]
      instrs -> instrs


compileA :: CoreExpr -> TimCompilerEnv -> TimAMode
compileA (EVar v) env = aLookup env v $ error $ "Unknown variable " ++ v
compileA (ENum n) env = IntConst n
compileA e        env = error "compileA: not a variable or constant"

compileU :: CoreExpr -> Int -> TimCompilerEnv -> OccupiedSlotIdx -> (OccupiedSlotIdx, TimAMode)
compileU (ENum n) u env d = (d, IntConst n)
compileU e        u env d = (d', Code (Compiled ns (PushMarker u:il)))
  where (d', Compiled ns il) = compileR e env d

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
                  , data_frame   = dfptr
                  , stack        = stk
                  , dump         = dmp
                  , heap         = from
                  , codes        = cstore
                  , stats        = sts
                  }
  = case evacuateStack cstore from hInitial stk of
  ((from1, to1), stk1) -> case evacuateDump cstore from1 to1 dmp of
    ((from2, to2), dmp1) -> case evacuateFramePtr True cstore from2 to2  (instrs, fptr) of
      ((from3, to3), fptr1) -> case evacuateFramePtr False cstore from3 to3 (instrs, dfptr) of
        ((from4, to4),  dfptr1) -> case scavenge from4 to4 of
          to5 -> let gcinfo = GCInfo { stepAt = statGetSteps sts
                                     , instr = instrs
                                     , stackInit = stk
                                     , fptrInit = fptr
                                     , dfptrInit = dfptr
                                     , heapBefore = from
                                     , heapEvacuatedByStack = (from1, to1)
                                     , heapEvacuatedByDump = (from2, to2)
                                     , heapEvacuatedByFramePtr = (from3, to3)
                                     , heapEvacuatedByDataFramePtr = (from4, to4)
                                     , heapScavenged = to5
                                     , fptrDone = fptr1
                                     , dfptrDone = dfptr1
                                     }
                 in applyToStats (statIncGCCount gcinfo)
                                $ state { frame = fptr1
                                        , data_frame = dfptr1
                                        , stack = stk1
                                        , dump = dmp1
                                        , heap = to5
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
            , iStr ">>> EVACUATED data frameptr: from4", iNewline
            , iStr "   ", iIndent (showHeap f4), iNewline, iNewline
            , iStr ">>> EVACUATED data frameptr: to4", iNewline
            , iStr "   ", iIndent (showHeap t4), iNewline, iNewline
            , iStr ">>> SCAVENGED: to5", iNewline
            , iStr "   ", iIndent (showHeap t5), iNewline, iNewline
            , iStr "new frame ptr: "
            , iIndent (showFramePtr fp'), iNewline
            , iStr "new data frame ptr: "
            , iIndent (showFramePtr dfp'), iNewline
            , iStr "^^^^^^^^^^^^^^^^^^^^^^^^", iNewline
            ]
  where f0 = heapBefore gcinfo
        (f1, t1) = heapEvacuatedByStack gcinfo
        (f2, t2) = heapEvacuatedByDump gcinfo
        (f3, t3) = heapEvacuatedByFramePtr gcinfo
        (f4, t4) = heapEvacuatedByDataFramePtr gcinfo
        t5 = heapScavenged gcinfo
        fp' = fptrDone gcinfo
        dfp' = dfptrDone gcinfo


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
        | not liveCheck ||
          holdInDataFrame instrs ||
          i `elem` go liveArgs = (hs, cls)
        | otherwise            = ((f, t), ([], FrameNull))
        where
          (hs, _) = evacuateFramePtr False cstore f t cls
          -- NOTE: ここで2段階以上の間接参照があるとスロットが GC されてしまう可能性がある
          -- 例えば [4 ~ [1,2], 2 ~ [3]] という状態で 4 が必要なら 3 も必要になる
          -- 循環参照もありえるので効率が悪いが停止条件は愚直にやっている
          go cur = if cur == new then cur else go new
            where new = nub . sort $ concatMap extract cur
          extract n = maybe [n] (n:) $ lookup n dict
      -- ReturnConstr があるなら data_frame で保持されてアクセスされうる
      -- アクセスする方の instruction は他から来るのでそこまでは解析せず全部保持する方針とした
      holdInDataFrame []                 = False
      holdInDataFrame (ReturnConstr _:_) = True
      holdInDataFrame (_:xs)             = holdInDataFrame xs

      liveArgs :: [Int]
      liveArgs = nub . sort $ foldl' g [] instrs
        where
          g ns (Move _ am)  = h ns am
          g ns (Push am)    = h ns am
          g ns (Enter am)   = h ns am
          g ns (Switch brs) = nub $ foldl' g ns (concatMap snd brs)
          g ns _            = ns

          h ns (Arg n)   = n:ns
          h ns (Data n)  = n:ns
          h ns (Code cs) = slotsOf cs ++ ns
          h ns (Label l) = ns
          h ns _         = ns

  -- Heap には含まないので from と to で変わらない
  FrameInt _  -> ((from, to), fptr)
  FrameNull   -> ((from, to), fptr)

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

evacuateDump :: CodeStore -> TimHeap -> TimHeap -> TimDump -> ((TimHeap, TimHeap), TimDump)
evacuateDump cstore from to dmp = case mapAccumL update (from, to) dmp of
  (hs, fpstks) -> (hs, zipWith (\(_, n', _) (fp, stk) -> (fp, n', stk)) dmp fpstks)
  where
    update :: (TimHeap, TimHeap) -> (FramePtr, Int, TimStack) -> ((TimHeap, TimHeap), (FramePtr, TimStack))
    update (f, t) (fp, n, stk) = ((f2, t2), (fp', stk'))
      where ((f1, t1), stk') = evacuateStack cstore f t stk
            ((f2, t2), fp') = evacuateFramePtr False cstore f1 t1 ([], fp)

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
                    , data_frame   = dfptr
                    , stack        = stk
                    , valstack     = vstk
                    , dump         = dmp
                    , heap         = hp
                    , codes        = cstore
                    , output       = out
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
          . clearOutputLast
          $ state)
    | otherwise -> error "Too few args for Take instruction"
    where
      (hp', fptr') = fAlloc hp (Frame (take n stk ++ replicate (t-n) ([], FrameNull)) [])
      stk' = drop n stk
  (Move n am:istr) -> applyToStats statIncExecTime
                      (putInstructions istr
                       . putHeap hp2
                       . clearOutputLast
                       $ state)
    where
      -- NOTE: Code 以外も処理されてしまうがコンパイラがバグってなければ問題ないはず
      hp1 = fUpdate hp fptr n (amToClosure am fptr dfptr hp cstore)
      hp2 = case am of
        Code cs -> fSetRelSlots hp1 fptr (n, slotsOf cs)
        _       -> hp1
  [Enter am]
    -> applyToStats statIncExecTime
       (putInstructions instr'
        . putFrame fptr'
        . putDataFrame FrameNull
        . clearOutputLast
        $ state)
    where
      (instr', fptr') = amToClosure am fptr dfptr hp cstore
  (Push am:istr)
    -> applyToStats statIncExecTime
       (putInstructions istr
        . putStack (amToClosure am fptr dfptr hp cstore : stk)
        . clearOutputLast
        $ state)
  (PushV fp:istr)
    -> applyToStats statIncExecTime $ case fp of
      FramePtr -> putInstructions istr
                  . putVStack (n:vstk)
                  . clearOutputLast
                  $ state
        where n = case fptr of
                FrameInt n' -> n'
                _           -> error "PushV applied to non-int frame"
      IntVConst n -> putInstructions istr
                     . putVStack (n:vstk)
                     . clearOutputLast
                     $ state
  (PushMarker x:istr)
    -> applyToStats statIncExecTime
       (putInstructions istr
        . putStack []
        . putDump ((fptr, x, stk):dmp)
        . clearOutputLast
        $ state)
  (UpdateMarkers n:istr)
    | m >= n    -> applyToStats statIncExecTime
                   (putInstructions istr
                    . clearOutputLast
                    $ state)
    | otherwise -> applyToStats statIncExecTime
                          (putStack (stk ++ s)
                           . putHeap hp'
                           . putDump dmp'
                           . clearOutputLast
                           $ state)
    where
      m = length stk
      ((fu, x, s), dmp') = case dmp of
        d:ds -> (d, ds)
        _    -> error "UpdateMarkers applied to empty dump"
      hp' = fUpdate h' fu x (i', f')
      (h', f') = fAlloc hp (Frame stk [(x, used_slots)]) -- NOTE: x slot depends on used_slots
      i' = map (Push . Arg) (reverse used_slots) ++ UpdateMarkers n:istr
      used_slots = [1..m]
  [Return] -> case stk of
    [] -> applyToStats statIncExecTime
          (putStack stk'
           . putDump dmp'
           . putHeap hp'
           . clearOutputLast
           $ state)
      where
        ((fu, x, stk'), dmp') = case dmp of
          d:ds -> (d, ds)
          _    -> error "Return applied to empty dump"
        n = case vstk of
          n:_ -> n
          _   -> error "Return applied to empty vstk"
        hp' = fUpdate hp fu x (intCode, FrameInt n)
    (instr', fptr'):stk' -> applyToStats statIncExecTime
                            (putInstructions instr'
                             . putFrame fptr'
                             . putDataFrame FrameNull
                             . putStack stk'
                             . clearOutputLast
                             $ state)
  [ReturnConstr t] -> case stk of
    [] -> applyToStats statIncExecTime
          (putStack stk'
           . putDump dmp'
           . putHeap hp'
           . clearOutputLast
           $ state)
      where
        ((fu, x, stk'), dmp') = case dmp of
          d:ds -> (d, ds)
          _    -> error "ReturnConstr applied to empty dump"
        f = getFrame state
        hp' = fUpdate hp fu x ([ReturnConstr t], f)
    _  -> applyToStats statIncExecTime
          (putInstructions i
           . putFrame f'
           . putDataFrame f
           . putStack stk'
           . putVStack vstk'
           . clearOutputLast
           $ state)
      where
        (i, f'):stk' = stk
        f = getFrame state
        vstk' = t:vstk
  (Op op:istr)
    -> applyToStats statIncExecTime
       (putInstructions istr
        . putVStack vstk'
        . clearOutputLast
        $ state)
    where
      vstk'
        | op `elem` [Add, Sub, Mul, Div, Eq, Ne, Lt, Le, Gt, Ge] = case vstk of
            n1:n2:ns -> op' n1 n2:ns
            _        -> error "Binary op applied to empty stack"
        | op == Neg = case vstk of
            n:ns -> negate n:ns
            _    -> error "Unary op applied to empty stack"
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
          b2i cmp x y = if x `cmp` y then 2 else 1 -- tag of true/false defined in extraPreludeDefs
  [Switch brs] -> applyToStats statIncExecTime
                  (putInstructions i
                   . putVStack vstk'
                   . clearOutputLast
                   $ state)
    where
      (t, vstk') = case vstk of
        n:ns -> (n, ns)
        _    -> error "Switch applied to empty stack"
      i = aLookup brs t $ error $ "no branch for " ++ show t
  (Print:istr) -> applyToStats statIncExecTime
                  (putInstructions istr
                   . putVStack vstk'
                   . putOutput (out' ++ [o], last_output)
                   $ state)
    where
      (o, vstk') = case vstk of
        n:ns -> (n, ns)
        _    -> error "Print applied to empty stack"
      (out', _)  = getOutput state
      last_output = case out' of
        []  -> iStr ("[" ++ show o)
        _:_ -> iStr ("," ++ show o)

  _ -> error $ "invalid instructions: " ++ show instrs

amToClosure :: TimAMode -> FramePtr -> FramePtr -> TimHeap -> CodeStore -> Closure
amToClosure (Arg n)      fptr dfptr heap cstore = fGet heap fptr n
amToClosure (Data n)     fptr dfptr heap cstore = fGet heap dfptr n
amToClosure (Code cs)    fptr dfptr heap cstore = (instrsOf cs, fptr)
amToClosure (Label l)    fptr dfptr heap cstore = (codeLookup cstore l, fptr)
amToClosure (IntConst n) fptr dfptr heap cstore = (intCode, FrameInt n)

intCode :: [Instruction]
intCode = [PushV FramePtr, Return]

showSimpleResult :: [TimState] -> String
showSimpleResult states = concatMap (iDisplay . outputLast . getOutput) states ++ "]"

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
                   , data_frame   = dfptr
                   , stack        = stk
                   , valstack     = vstk
                   , dump         = dmp
                   , heap         = hp
                   , codes        = cs
                   , output       = out
                   , stats        = stat
                   }
  = iConcat $ [ iStr "Code:  "
              , showInstructions Terse is, iNewline
              , iStr "Frame: "
              , showFrame hp fptr, iNewline
              , iStr "Data frame: "
              , showFrame hp dfptr, iNewline
              , iStr "Rel slots: "
              , showRelSlots hp fptr, iNewline
              , iStr "Arg stack: "
              , showStack stk, iNewline
              , iStr "Value stack: "
              , showValueStack vstk, iNewline
              , iStr "Dump: "
              , showDump dmp, iNewline
              , iStr "Output: "
              , showOutput out, iNewline
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
showInstructions None _ = iStr "{..}"
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
showInstruction _ (Take t n)        = iConcat [iStr "Take ", iNum t, iStr " ", iNum n]
showInstruction d (Move n a)        = iConcat [iStr "Move ", iNum n, iStr " ", showArg d a]
showInstruction d (Enter x)         = iStr "Enter " `iAppend` showArg d x
showInstruction d (Push x)          = iStr "Push " `iAppend` showArg d x
showInstruction _ (PushV x)         = iStr "PushV " `iAppend` showValueAMode x
showInstruction _ (PushMarker x)    = iStr "PushMarker " `iAppend` iNum x
showInstruction _ (UpdateMarkers n) = iStr "UpdateMarkers " `iAppend` iNum n
showInstruction _ Return            = iStr "Return"
showInstruction _ (ReturnConstr n)  = iStr "ReturnConstr " `iAppend` iNum n
showInstruction _ (Op op)           = iStr "Op " `iAppend` iStr (show op)
showInstruction _ Print             = iStr "Print"
showInstruction d (Switch brs)      = iConcat [ iStr "Switch ["
                                              , iIndent (iInterleave sep (map (showBranch d) brs))
                                              , iStr "]"
                                              ]
  where sep = iStr "," `iAppend` iNewline

showBranch :: HowMuchToPrint -> Branch -> IseqRep
showBranch d (tag, instrs) = iConcat [ iNum tag, iStr " -> ", showInstructions d instrs ]

showValueAMode :: ValueAMode -> IseqRep
showValueAMode FramePtr      = iStr "FramePtr"
showValueAMode (IntVConst n) = iStr "IntVConst " `iAppend` iNum n

showArg :: HowMuchToPrint -> TimAMode -> IseqRep
showArg _ (Arg n)   = iStr "Arg " `iAppend` iNum n
showArg _ (Data n)  = iStr "Data " `iAppend` iNum n
showArg d (Code il) = iConcat [ iStr "Code "
                              , showUsedSlots ns
                              , iStr " "
                              , showInstructions d instrs
                              ]
  where Compiled ns instrs = il
showArg _ (Label s) = iStr "Label " `iAppend` iStr s
showArg _ (IntConst n) = iStr "IntConst " `iAppend` iNum n

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

showOutput :: TimOutput -> IseqRep
showOutput (out, _) = iConcat [ iStr "["
                              , iIndent (iInterleave (iStr ",") (map iNum out))
                              , iStr "]"
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
