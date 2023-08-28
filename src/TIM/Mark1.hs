{-# LANGUAGE FlexibleInstances #-}
module TIM.Mark1
  ( parse
  , compile
  , eval
  , showResults
  , runProg
  ) where

import Heap
import Iseq
import Language
import Utils

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

data TimStats
  = TimStats { getSteps :: Int  -- The number of steps
             , getMaxStackDepth :: Int  -- The maximum stack depth
             }
  deriving (Eq, Show)

statInitial :: TimStats
statInitial = TimStats { getSteps = 0, getMaxStackDepth = 0 }

statGetSteps :: TimStats -> Int
statGetSteps s = getSteps s

statSetSteps :: Int -> TimStats -> TimStats
statSetSteps n sts = sts { getSteps = n }

statIncSteps :: TimStats -> TimStats
statIncSteps sts = sts { getSteps = getSteps sts + 1 }

statGetMaxStackDepth :: TimStats -> Int
statGetMaxStackDepth s = getMaxStackDepth s

statUpdateMaxStackDepth :: Int -> TimStats -> TimStats
statUpdateMaxStackDepth depth s
  | depth > depth' = s { getMaxStackDepth = depth }
  | otherwise      = s
  where depth' = statGetMaxStackDepth s



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
doAdmin state = applyToStats statIncSteps state

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
      -> putInstructions instr
         . putFrame fptr'
         . putStack stk'
         . putHeap hp'
         $ state
    | otherwise       -> error "Too few args for Take instruction"
    where
      (hp', fptr') = fAlloc hp (take n stk)
      stk' = drop n stk
  [Enter am]
    -> putInstructions instr'
       . putFrame fptr'
       $ state
    where
      (instr', fptr') = amToClosure am fptr hp cstore
  (Push am:istr)
    -> putInstructions istr
       . putStack (amToClosure am fptr hp cstore : stk)
       $ state
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
            , iIndent (iInterleave iNewline (map showClosure (fList (hLookup heap addr))))
            , iStr ">", iNewline
            ]
showFrame heap (FrameInt n)
  = iConcat [ iStr "Frame ptr (int): ", iNum n, iNewline ]

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
showFramePtr (FrameAddr a) = iStr (show a)
showFramePtr (FrameInt n)  = iStr "int " `iAppend` iNum n

showStats :: TimState -> IseqRep
showStats state@TimState { heap = hp,  stats = stats }
  = iConcat [ iStr "Steps taken = ", iNum (statGetSteps stats), iNewline
            , iStr "No of frames allocated = ", iNum (hSize hp), iNewline
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
