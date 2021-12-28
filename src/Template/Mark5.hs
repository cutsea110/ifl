module Template.Mark5
  ( parse
  , eval
  , compile
  , showResults
  ) where

import Data.List (mapAccumL)

import Iseq
import Language
import Heap
import Stack
import Utils

data Primitive
  = Neg
  | Add
  | Sub
  | Mul
  | Div
  deriving Show

data Node
  = NAp Addr Addr
  | NSupercomb Name [Name] CoreExpr
  | NNum Int
  | NInd Addr
  | NPrim Name Primitive

primitives :: Assoc Name Primitive
primitives = [ ("negate", Neg)
             , ("+", Add)
             , ("-", Sub)
             , ("*", Mul)
             , ("/", Div)
             ]

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = Stack Addr
type TiDump = Stack TiStack
initialTiDump :: TiDump
initialTiDump = emptyStack
type TiHeap = Heap Node
type TiGlobals = Assoc Name Addr
type TiStats = (Int, Int, Int)
tiStatInitial :: TiStats
tiStatInitial = (0, 0, 0)
tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps (ttl, sc, p) = (ttl+1, sc, p)
tiStatIncScSteps :: TiStats -> TiStats
tiStatIncScSteps (ttl, sc, p) = (ttl, sc+1, p)
tiStatIncPrimSteps :: TiStats -> TiStats
tiStatIncPrimSteps (ttl, sc, p) = (ttl, sc, p+1)
tiStatGetSteps :: TiStats -> Int
tiStatGetSteps (ttl, _, _) = ttl
tiStatGetScSteps :: TiStats -> Int
tiStatGetScSteps (_, sc, _) = sc
tiStatGetPrimSteps :: TiStats -> Int
tiStatGetPrimSteps (_, _, p) = p
applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (stack, dump, heap, scDefs, stats)
  = (stack, dump, heap, scDefs, f stats)

compile :: CoreProgram -> TiState
compile program = (initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
  where
    scDefs = program ++ preludeDefs ++ extraPreludeDefs
    (initialHeap, globals) = buildInitialHeap scDefs
    initialStack = fromList [addressOfMain]
    addressOfMain = aLookup globals "main" (error "main is not defined")

extraPreludeDefs :: CoreProgram
extraPreludeDefs = []

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs = (heap', env ++ env')
  where (heap,  env ) = mapAccumL allocateSc hInitial scDefs
        (heap', env') = mapAccumL allocatePrim heap primitives

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
  where
    (heap', addr) = hAlloc heap (NSupercomb name args body)

allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocatePrim heap (name, prim) = (heap', (name, addr))
  where (heap', addr) = hAlloc heap (NPrim name prim)

-- | Ex 2.9 最後に TiFinal でエラーになったときの state まで取り出せるが
--   提案されているものでは取り出せない
eval :: TiState -> [TiState]
eval state = state : restStates
  where
    restStates
      | tiFinal state = []
      | otherwise     = eval nextState
    nextState = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state

doAdminSc :: TiState -> TiState
doAdminSc state = applyToStats tiStatIncScSteps state

doAdminPrim :: TiState -> TiState
doAdminPrim state = applyToStats tiStatIncPrimSteps state

tiFinal :: TiState -> Bool
tiFinal (stack, dump, heap, _, _) = case getStack stack of
  [soleAddr] -> isDataNode (hLookup heap soleAddr) && isEmpty dump
  []         -> error "Empty stack"
  _          -> False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

isIndNode :: Node -> Bool
isIndNode (NInd _) = True
isIndNode _        = False

step :: TiState -> TiState
step state@(stack, dump, heap, globals, stats) = dispatch (hLookup heap item)
  where
    (item, stack') = pop stack
    dispatch (NNum n)                  = numStep state n
    dispatch (NAp a1 a2)               = apStep state a1 a2
    dispatch (NSupercomb sc args body) = doAdminSc $ scStep state sc args body
    dispatch (NInd a)                  = indStep state a
    dispatch (NPrim name prim)         = doAdminPrim $ primStep state prim

numStep :: TiState -> Int -> TiState
numStep (stack, dump, heap, globals, stats) n
  | isEmpty stack = error "numStep: empty stack."
  | otherwise     = (stack', dump', heap, globals, stats)
  where (stack', dump') = pop dump

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2
  | isIndNode a2node = (stack,  dump, heap', globals, stats)
  | otherwise        = (stack', dump, heap,  globals, stats)
  where stack' = push a1 stack
        (a0, _) = pop stack
        a2node = hLookup heap a2
        NInd a3 = a2node
        heap' = hUpdate heap a0 (NAp a1 a3)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) scName argNames body
  | getDepth stack < length argNames + 1 = error "Too few arguments given"
  | otherwise = (stack', dump, heap', globals, stats)
  where
    argsLen = length argNames
    stack' = discard argsLen stack
    (root, _) = pop stack'
    heap' = instantiateAndUpdate body root heap (bindings ++ globals)
    bindings = zip argNames (getargs heap stack)

indStep :: TiState -> Addr -> TiState
indStep (stack, dump, heap, globals, stats) a = (stack', dump, heap, globals, stats)
  where stack' = push a (discard 1 stack)

primStep :: TiState -> Primitive -> TiState
primStep state Neg = primNeg state
primStep state Add = primArith state (+)
primStep state Sub = primArith state (-)
primStep state Mul = primArith state (*)
primStep state Div = primArith state div

primNeg :: TiState -> TiState
primNeg (stack, dump, heap, globals, stats)
  | length args /= 1 = error "primNeg: wrong number of args."
  | isDataNode argnode = (stack', dump, heap', globals, stats)
  | otherwise = (push argaddr emptyStack, push stack dump, heap, globals, stats)
  where args = getargs heap stack
        [argaddr] = args
        argnode = hLookup heap argaddr
        NNum n = argnode
        (_, stack') = pop stack
        (root, _)   = pop stack'
        heap' = hUpdate heap root (NNum (negate n))

primArith :: TiState -> (Int -> Int -> Int) -> TiState
primArith (stack, dump, heap, globals, stats) op
  | length args /= 2 = error "primArith: wrong number of args."
  | not (isDataNode lnode) = (push laddr emptyStack, dump', heap, globals, stats)
  | not (isDataNode rnode) = (push raddr emptyStack, dump', heap, globals, stats)
  | otherwise = (stack', dump, heap', globals, stats)
  where args = getargs heap stack
        [laddr, raddr] = args
        [lnode, rnode] = map (hLookup heap) args
        (NNum m, NNum n) = (lnode, rnode)
        stack' = discard 2 stack
        dump' = push stack' dump
        (root, _) = pop stack'
        heap' = hUpdate heap root (NNum (m `op` n))

instantiateAndUpdate :: CoreExpr -> Addr -> TiHeap -> Assoc Name Addr -> TiHeap
instantiateAndUpdate (ENum n)               updAddr heap env = hUpdate heap updAddr (NNum n)
instantiateAndUpdate (EAp e1 e2)            updAddr heap env = hUpdate heap2 updAddr (NAp a1 a2)
  where (heap1, a1) = instantiate e1 heap  env
        (heap2, a2) = instantiate e2 heap1 env
instantiateAndUpdate (EVar v)               updAddr heap env = hUpdate heap updAddr (NInd varAddr)
  where varAddr = aLookup env v (error $ "Undefined name " ++ show v)
instantiateAndUpdate (ELet isrec defs body) updAddr heap env = instantiateAndUpdate body updAddr heap' env'
  where (heap', extraBindings) = mapAccumL instantiateRhs heap defs
        env' = extraBindings ++ env
        rhsEnv | isrec     = env'
               | otherwise = env
        instantiateRhs heap (name, rhs) = (heap', (name, addr))
          where (heap', addr) = instantiate rhs heap rhsEnv
instantiateAndUpdate (EConstr tag arity)    updAddr heap env = error "TODO: implement instantiateAndUpdate"
instantiateAndUpdate _                      updAddr heap env = error "not yet implemented"

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap stack = case getStack stack of
  []       -> error "Empty stack"
  _:stack' -> map getarg stack'
    where getarg addr = arg
            where NAp _fun arg = hLookup heap addr

instantiate :: CoreExpr -> TiHeap -> Assoc Name Addr -> (TiHeap, Addr)
instantiate (ENum n)               heap env = hAlloc heap (NNum n)
instantiate (EAp e1 e2)            heap env = hAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap  env
    (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v)               heap env = (heap, aLookup env v (error ("Undefined name " ++ show v)))
instantiate (EConstr tag arity)    heap env = instantiateConstr tag arity heap env
instantiate (ELet isrec defs body) heap env = instantiateLet isrec defs body heap env
instantiate (ECase e alts)         heap env = error "Can't instantiate case exprs"
instantiate (ELam vs e)            heap env = error "Can't instantiate lambda abstractions"

instantiateConstr :: Tag -> Arity -> TiHeap -> Assoc Name Addr -> (TiHeap, Addr)
instantiateConstr = error "TODO: implement instantiateConstr"

instantiateLet :: IsRec -> [(Name, CoreExpr)] -> CoreExpr -> TiHeap -> Assoc Name Addr -> (TiHeap, Addr)
instantiateLet isrec defs expr heap env = instantiate expr heap' env'
  where
    (heap', extraBindings) = mapAccumL instantiateRhs heap defs
    env' = extraBindings ++ env
    rhsEnv | isrec     = env'
           | otherwise = env
    instantiateRhs heap (name, rhs) = (heap1, (name, addr))
      where (heap1, addr) = instantiate rhs heap rhsEnv

------

showResults :: [TiState] -> String
showResults states
  = iDisplay (iConcat [ iLayn (map showState states)
                      , showStats lastState
                      , showAllocCount lastState
                      , showStackMaxDepth lastState
                      , showDumpMaxDepth lastState
                      ])
  where
    lastState = last states

showDumpMaxDepth :: TiState -> IseqRep
showDumpMaxDepth (_, dump, _, _, _)
  = iConcat [ iNewline, iStr "   Dump maximum depth = "
            , iNum (getHighWaterMark dump)
            ]

showStackMaxDepth :: TiState -> IseqRep
showStackMaxDepth (stack, _, _, _, _)
  = iConcat [ iNewline, iStr "  Stack maximum depth = "
            , iNum (getHighWaterMark stack)
            ]

showAllocCount :: TiState -> IseqRep
showAllocCount (_, _, (allocs, _, _, _), _, _)
  = iConcat [ iNewline, iStr "     Allocation count = "
            , iNum allocs
            ]

showState :: TiState -> IseqRep
showState (stack, dump, heap, globals, stats)
  = iConcat [ showHeap heap, iNewline
            , showStack heap stack, iNewline
            , showDumpDepth dump, iNewline
            ]

showHeap :: TiHeap -> IseqRep
showHeap (_, _, _, cts)
  = iConcat [ iStr "Heap  ["
            , iIndent (iInterleave iNewline $ map showHeapItem cts)
            , iStr " ]"
            ]
  where
    showHeapItem (addr, node)
      = iConcat [ showFWAddr addr, iStr ": "
                , showNode node
                ]

showStack :: TiHeap -> TiStack -> IseqRep
showStack heap stack
  = iConcat
    [ iStr "Stack ["
    , iIndent (iInterleave iNewline (map showStackItem (getStack stack)))
    , iStr " ]"
    ]
  where
    showStackItem addr
      = iConcat [ showFWAddr addr, iStr ": "
                , showStkNode heap (hLookup heap addr)
                ]

showDumpDepth :: TiDump -> IseqRep
showDumpDepth dump
  = iConcat [ iStr "Dump Depth "
            , iStr (show $ getDepth dump)
            ]

showStkNode :: TiHeap -> Node -> IseqRep
showStkNode heap (NAp funAddr argAddr)
  = iConcat [ iStr "NAp ", showFWAddr funAddr
            , iStr " ", showFWAddr argAddr
            , iStr " (", showNode (hLookup heap argAddr), iStr ")"
            ]
showStkNode heap node = showNode node

showNode :: Node -> IseqRep
showNode (NAp a1 a2)           = iConcat [ iStr "NAp ", showAddr a1, iStr " ", showAddr a2 ]
showNode (NSupercomb name _ _) = iStr ("NSupercomb " ++ name)
showNode (NNum n)              = iStr "NNum " `iAppend` iNum n
showNode (NInd a)              = iStr "NInd " `iAppend` showAddr a
showNode (NPrim _ prim)        = iStr "NPrim " `iAppend` iStr (show prim)

showAddr :: Addr -> IseqRep
showAddr addr = iStr (showaddr addr)

showFWAddr :: Addr -> IseqRep
showFWAddr addr = iStr (space (4 - length str) ++ str)
  where str = show addr

showStats :: TiState -> IseqRep
showStats (stack, dump, heap, globals, stats)
  = iConcat [ iNewline
            , iNewline, iStr "Total number of steps = "
            , iNum (tiStatGetSteps stats)
            , iNewline, iStr "Supercombinator steps = "
            , iNum (tiStatGetScSteps stats)
            , iNewline, iStr "      Primitive steps = "
            , iNum (tiStatGetPrimSteps stats)
            ]
