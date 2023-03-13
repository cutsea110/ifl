{-# LANGUAGE NPlusKPatterns  #-}
module Gmachine.Mark4
  ( parse
  , compile
  , eval
  , showResults
  , runProg
  ) where

import Heap
import Iseq
import Language
import qualified Stack as S
import Utils
import Data.List (mapAccumL)

runProg :: String -> String
runProg = showResults . eval . compile . parse

data GmState = GmState { code    :: GmCode
                       , stack   :: GmStack
                       , dump    :: GmDump
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
  | Pop Int  -- pop offset
  | Update Int -- update offset by stack top
  | Slide Int
  | Alloc Int
  | Eval
  | Add | Sub | Mul | Div | Neg
  | Eq | Ne | Lt | Le | Gt | Ge
  | Cond GmCode GmCode
  deriving (Eq, Show)

type GmStack = S.Stack Addr

getStack :: GmState -> GmStack
getStack state = stack state

putStack :: GmStack -> GmState -> GmState
putStack stack' state = state { stack = stack' }

type GmDump = S.Stack GmDumpItem
type GmDumpItem = (GmCode, GmStack)

getDump :: GmState -> GmDump
getDump state = dump state

putDump :: GmDump -> GmState -> GmState
putDump dump' state = state { dump = dump' }

type GmHeap = Heap Node

getHeap :: GmState -> GmHeap
getHeap state = heap state

putHeap :: GmHeap -> GmState -> GmState
putHeap heap' state = state { heap = heap' }

data Node
  = NNum Int           -- Numbers
  | NAp Addr Addr      -- Applications
  | NGlobal Int GmCode -- Globals
  | NInd Addr          -- Indirections
  deriving Show


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
dispatch (Pop n)        = pop n
dispatch (Update n)     = update n
dispatch Unwind         = unwind
dispatch (Slide n)      = slide n
dispatch (Alloc n)      = alloc n
dispatch Eval           = evalop
dispatch Add            = arithmetic2 (+)
dispatch Sub            = arithmetic2 (-)
dispatch Mul            = arithmetic2 (*)
dispatch Div            = arithmetic2 div
dispatch Neg            = arithmetic1 negate
dispatch Eq             = comparison (==)
dispatch Ne             = comparison (/=)
dispatch Lt             = comparison (<)
dispatch Le             = comparison (<=)
dispatch Gt             = comparison (>)
dispatch Ge             = comparison (>=)
dispatch (Cond t e)     = cond t e

evalop :: GmState -> GmState
evalop state = state { code = [Unwind]
                     , stack = S.push a S.emptyStack
                     , dump = S.push (i, s) d
                     }
  where d = getDump state
        (a, s) = S.pop (getStack state)
        i = getCode state


boxInteger :: Int -> GmState -> GmState
boxInteger n state
  = putStack (S.push a stack) (putHeap h' state)
  where stack = getStack state
        (h', a) = hAlloc (getHeap state) (NNum n)

unboxInteger :: Addr -> GmState -> Int
unboxInteger a state
  = ub (hLookup (getHeap state) a)
  where ub (NNum i) = i
        ub n        = error "Unboxing a non-integer"

primitive1 :: (b -> GmState -> GmState) -- boxing function
           -> (Addr -> GmState -> a)    -- unboxing function
           -> (a -> b)                  -- operator
           -> (GmState -> GmState)      -- state transition
primitive1 box unbox op state
  = box (op (unbox a state)) (putStack as state)
  where (a, as) = S.pop $ getStack state

primitive2 :: (b -> GmState -> GmState) -- boxing function
           -> (Addr -> GmState -> a)    -- unboxing function
           -> (a -> a -> b)             -- operator
           -> (GmState -> GmState)      -- state transition
primitive2 box unbox op state
  = box (op (unbox a0 state) (unbox a1 state)) (putStack as1 state)
    where stack = getStack state
          (a0, as0) = S.pop stack
          (a1, as1) = S.pop as0

arithmetic1 :: (Int -> Int)         -- arithmetic operator
            -> (GmState -> GmState) -- state transition
arithmetic1 = primitive1 boxInteger unboxInteger

arithmetic2 :: (Int -> Int -> Int)  -- arithmetic operator
            -> (GmState -> GmState) -- state transition
arithmetic2 = primitive2 boxInteger unboxInteger

boxBoolean :: Bool -> GmState -> GmState
boxBoolean b state
  = putStack (S.push a stack) (putHeap h' state)
  where stack = getStack state
        (h', a) = hAlloc (getHeap state) (NNum b')
        b' | b         = 1
           | otherwise = 0

comparison :: (Int -> Int -> Bool) -> GmState -> GmState
comparison = primitive2 boxBoolean unboxInteger

cond :: GmCode -> GmCode -> GmState -> GmState
cond i1 i2 state = case hLookup heap a of
  NNum 1 -> putCode (i1 ++ i) $ putStack stack' state
  NNum 0 -> putCode (i2 ++ i) $ putStack stack' state
  where heap = getHeap state
        stack = getStack state
        i = getCode state
        (a, stack') = S.pop stack

pushglobal :: Name -> GmState -> GmState
pushglobal f state = putStack (S.push a $ getStack state) state
  where a = aLookup (getGlobals state) f (error $ "Undeclared global " ++ f)

pushint :: Int -> GmState -> GmState
pushint n state = case aLookup (getGlobals state) name (-1) of
  a' | a' < 0    -> state { stack = S.push a (getStack state)
                          , heap = heap'
                          , globals = aInsert (getGlobals state) name a'
                          }
     | otherwise -> state { stack = S.push a' (getStack state)
                          }
  where name = show n
        (heap', a) = hAlloc (getHeap state) (NNum n)

mkap :: GmState -> GmState
mkap state = putHeap heap' (putStack (S.push a s2) state)
  where (heap', a)  = hAlloc (getHeap state) (NAp a1 a2)
        (a1, s1) = S.pop $ getStack state
        (a2, s2) = S.pop s1

push :: Int -> GmState -> GmState
push n state = putStack (S.push a s) state
  where s = getStack state
        a = S.getStack s !! n

pop :: Int -> GmState -> GmState
pop n state = putStack (S.discard n s) state
  where s = getStack state

update :: Int -> GmState -> GmState
update n state = putHeap heap' (putStack s' state)
  where s = getStack state
        (a, s') = S.pop s
        a' = S.getStack s' !! n
        heap' = hUpdate (getHeap state) a' (NInd a)

slide :: Int -> GmState -> GmState
slide n state = putStack (S.push a $ S.discard n s) state
  where (a, s) = S.pop $ getStack state

alloc :: Int -> GmState -> GmState
alloc n state = state { stack = stack2
                      , heap  = heap2
                      }
  where heap1 = getHeap state
        (heap2, addrs) = allocNodes n heap1
        stack1 = getStack state
        stack2 = foldr S.push stack1 addrs
        
allocNodes :: Int -> GmHeap -> (GmHeap, [Addr])
allocNodes 0     heap = (heap, [])
allocNodes (n+1) heap = (heap2, a:as)
  where (heap1, as) = allocNodes n heap
        (heap2, a ) = hAlloc heap1 (NInd hNull)
allocNodes _ _        = error "allocNodes: negative"

getArg :: Node -> Addr
getArg (NAp _ a2) = a2
getArg _          = error "not application Node"

rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap as = foldr S.push (S.discard n as) $ take n as'
  where
    (_, s) = S.pop as
    as' = map (getArg . hLookup heap) (S.getStack s)

unwind :: GmState -> GmState
unwind state = newState (hLookup heap a)
  where s = getStack state
        (a, s1) = S.pop s
        heap   = getHeap state
        dump   = getDump state
        newState (NNum n) | S.isEmpty dump = putCode [] state
                          | otherwise      = state { code = i'
                                                   , stack = S.push a s'
                                                   , dump = d
                                                   }
          where ((i', s'), d) = S.pop $ getDump state
        newState (NAp a1 a2) = putCode [Unwind] (putStack (S.push a1 s) state)
        newState (NGlobal n c)
          | S.getDepth s1 < n = error "Unwinding with too few arguments"
          | otherwise         = putCode c (putStack (rearrange n (getHeap state) (getStack state)) state)
        newState (NInd a1) = putCode [Unwind] (putStack (S.push a1 s1) state)

compile :: CoreProgram -> GmState
compile program = GmState { code = initialCode
                          , stack = S.emptyStack
                          , dump = S.emptyStack
                          , heap = heap
                          , globals = globals
                          , stats = statInitial
                          }
  where (heap, globals) = buildInitialHeap program

buildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildInitialHeap program = mapAccumL allocateSc hInitial compiled
  where
    compiled = map compileSc (preludeDefs ++ program) ++ compiledPrimitives
    -- compiled = map compileSc program

compiledPrimitives :: [GmCompiledSC]
compiledPrimitives
  = [ ("+", 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind])
    , ("-", 2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind])
    , ("*", 2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind])
    , ("/", 2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind])
    , ("negate", 1, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind])

    , ("==", 2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind])
    , ("/=", 2, [Push 1, Eval, Push 1, Eval, Ne, Update 2, Pop 2, Unwind])
    , ("<",  2, [Push 1, Eval, Push 1, Eval, Lt, Update 2, Pop 2, Unwind])
    , ("<=", 2, [Push 1, Eval, Push 1, Eval, Le, Update 2, Pop 2, Unwind])
    , (">",  2, [Push 1, Eval, Push 1, Eval, Gt, Update 2, Pop 2, Unwind])
    , (">=", 2, [Push 1, Eval, Push 1, Eval, Ge, Update 2, Pop 2, Unwind])
    , ("if", 3, [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])
    ]

type GmCompiledSC = (Name, Int, GmCode)

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns) = (heap', (name, addr))
  where (heap', addr) = hAlloc heap (NGlobal nargs instns)

initialCode :: GmCode
initialCode = [Pushglobal "main", Eval]

{- |
>>> compileSc ("K", ["x", "y"], EVar "x")
("K",2,[Push 0,Update 2,Pop 2,Unwind])

>>> compileSc ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x")))
("S",3,[Push 2,Push 2,Mkap,Push 3,Push 2,Mkap,Mkap,Update 3,Pop 3,Unwind])
-}
compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC
compileSc (name, env, body) = (name, length env, compileR body (zip env [0..]))

-- maybe Reduction's R
compileR :: GmCompiler
compileR e env = compileC e env ++ [Update n, Pop n, Unwind]
  where n = length env

type GmCompiler = CoreExpr  -> GmEnvironment -> GmCode
type GmEnvironment = Assoc Name Int

-- to Code
compileC :: GmCompiler
compileC (EVar v) env
  | v `elem` aDomain env = [Push n]
  | otherwise            = [Pushglobal v]
  where n = aLookup env v (error "Can't happen")
compileC (ENum n)    env = [Pushint n]
compileC (EAp e1 e2) env = compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [Mkap]
compileC (ELet recursive defs e) env
  | recursive            = compileLetrec compileC defs e env
  | otherwise            = compileLet    compileC defs e env

compileLet :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLet comp defs expr env
  = compileLet' defs env ++ comp expr env' ++ [Slide (length defs)]
  where env' = compileArgs defs env

compileLet' :: [(Name, CoreExpr)] -> GmEnvironment -> GmCode
compileLet' []                  env = []
compileLet' ((name, expr):defs) env
  = compileC expr env ++ compileLet' defs (argOffset 1 env)

compileArgs :: [(Name, CoreExpr)] -> GmEnvironment -> GmEnvironment
compileArgs defs env
  = zip (aDomain defs) [n-1, n-2 .. 0] ++ argOffset n env
  where n = length defs

{- |
>>> compileSc . head . parse $ "Y f = letrec x = f x in x"
("Y",1,[Alloc 1,Push 0,Push 2,Mkap,Update 0,Push 0,Slide 1,Update 1,Pop 1,Unwind])
-}
compileLetrec :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLetrec comp defs expr env
  = [Alloc n] ++ compiled defs ++ comp expr env' ++ [Slide n]
  where n = length defs
        env' = compileArgs defs env
        compiled dds = fst $ foldr phi ([], 0) dds
          where phi (_, e) (ds, i) = (compileC e env' ++ [Update i] ++ ds, i+1)

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(v, n+m) | (v, m) <- env]

showResults :: [GmState] -> String
showResults states
  = unlines (map iDisplay
              ([ iStr "Supercombinator definitions", iNewline
               , iInterleave iNewline (map (showSC s) (getGlobals s))
               , iStr "State transitions"
               ] ++
               iLayn' (map showState states) ++
               [ showStats (last states)
               ])
            )
    where s:ss = states


-- | show dump list
showSC :: GmState -> (Name, Addr) -> IseqRep 
showSC s (name, addr)
  = iConcat [ iStr "Code for ", iStr name, iNewline
            , showInstructions code, iNewline, iNewline
            ]
  where (NGlobal arity code) = hLookup (getHeap s) addr

showInstructions :: GmCode -> IseqRep
showInstructions is
  = iConcat [ iStr "  Code:{"
            , iIndent (iInterleave iNewline (map showInstruction is))
            , iStr "}", iNewline
            ]

showInstruction :: Instruction -> IseqRep
showInstruction Unwind         = iStr "Unwind"
showInstruction (Pushglobal f) = iStr "Pushglobal " `iAppend` iStr f
showInstruction (Push n)       = iStr "Push " `iAppend` iNum n
showInstruction (Pushint n)    = iStr "Pushint " `iAppend` iNum n
showInstruction Mkap           = iStr "Mkap"
showInstruction (Pop n)        = iStr "Pop " `iAppend` iNum n
showInstruction (Update n)     = iStr "Update " `iAppend` iNum n
showInstruction (Slide n)      = iStr "Slide " `iAppend` iNum n
showInstruction (Alloc n)      = iStr "Alloc " `iAppend` iNum n
showInstruction Eval           = iStr "Eval"
showInstruction Add            = iStr "Add"
showInstruction Sub            = iStr "Sub"
showInstruction Mul            = iStr "Mul"
showInstruction Div            = iStr "Div"
showInstruction Neg            = iStr "Neg"
showInstruction Eq             = iStr "Eq"
showInstruction Ne             = iStr "Ne"
showInstruction Lt             = iStr "Lt"
showInstruction Le             = iStr "Le"
showInstruction Gt             = iStr "Gt"
showInstruction Ge             = iStr "Ge"
showInstruction (Cond t e)     = iStr "Cond " `iAppend` showCodes t `iAppend` showCodes e
  where showCodes [] = iStr "[]"
        showCodes (x:xs) = iConcat [iStr "[ ", showInstruction x, iStr ".. ]"]

showState :: GmState -> IseqRep
showState s
  = iConcat [ showStack s, iNewline
            , showDump s, iNewline
            , showInstructions (getCode s), iNewline 
            ]

showDump :: GmState -> IseqRep
showDump s
  = iConcat [ iStr "  Dump: ["
            , iIndent (iInterleave iNewline
                      (map showDumpItem (reverse (S.getStack $ getDump s))))
            , iStr "]"
            ]

showDumpItem :: GmDumpItem -> IseqRep
showDumpItem (code, stack)
  = iConcat [ iStr "<"
            , shortShowInstructions 3 code, iStr ", "
            , shortShowStack stack
            , iStr ">"
            ]

shortShowInstructions :: Int -> GmCode -> IseqRep
shortShowInstructions number code
  = iConcat [ iStr "{"
            , iInterleave (iStr "; ") dotcodes
            , iStr "}"
            ]
    where codes = map showInstruction (take number code)
          dotcodes | length code > number = codes ++ [iStr "..."]
                   | otherwise            = codes

shortShowStack :: GmStack -> IseqRep
shortShowStack stack
  = iConcat [ iStr "["
            , iInterleave (iStr ", ") (map (iStr . showaddr) $ S.getStack stack)
            , iStr "]"
            ]

showStack :: GmState -> IseqRep
showStack s
  = iConcat [ iStr "Stack: ["
            , iIndent $ iInterleave iNewline items
            , iStr "]"
            ]
    where
      items = map (showStackItem s) (reverse (S.getStack $ getStack s))

showStackItem :: GmState -> Addr -> IseqRep
showStackItem s a
  = iConcat [ iStr (showaddr a), iStr ": "
            , showNode s a (hLookup (getHeap s) a)
            ]

showNode :: GmState -> Addr -> Node -> IseqRep
showNode s a (NNum n)      = iNum n
showNode s a (NGlobal n g) = iConcat [iStr "Global ", iStr v]
  where v = head [n | (n, b) <- getGlobals s, a == b]
showNode s a (NAp a1 a2)   = iConcat [ iStr "Ap ", iStr (showaddr a1)
                                     , iStr " ", iStr (showaddr a2)
                                     ]
showNode s a (NInd a1) = iConcat [ iStr "NInd ", iStr (showaddr a1)
                                 ]

showStats :: GmState -> IseqRep
showStats s = iConcat [ iStr "---------------"
                      , iNewline
                      , iNewline, iStr "Total number of steps = "
                      , iNum (statGetSteps (getStats s))
                      , iNewline, iStr "            Heap size = "
                      , iNum (hSize (heap s))
                      , iNewline, iStr "           Stack size = "
                      , iNum (S.getHighWaterMark (getStack s))
                      ]

{- |
>>> let i = head $ S.getStack $ getStack $ last $ eval $ compile $ parse "main = S K K 3"
>>> hLookup (getHeap $ last $ eval $ compile $ parse "main = S K K 3") i
NNum 3
-}
