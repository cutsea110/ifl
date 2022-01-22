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
  | PrimConstr Tag Arity
  | If
  | Eq | NotEq
  | Less | LessEq
  | Greater | GreaterEq
  | PrimCasePair
  | PrimCaseList
  | Abort
  | Print
  | Stop
  deriving Show

data Node
  = NAp Addr Addr
  | NSupercomb Name [Name] CoreExpr
  | NNum Int
  | NInd Addr
  | NPrim Name Primitive
  | NData Int [Addr]

primitives :: Assoc Name Primitive
primitives = [ ("negate", Neg)
             , ("+", Add)
             , ("-", Sub)
             , ("*", Mul)
             , ("/", Div)
             , ("if", If)
             , ("==", Eq)
             , ("/=", NotEq)
             , ("<", Less)
             , ("<=", LessEq)
             , (">", Greater)
             , (">=", GreaterEq)
             , ("casePair", PrimCasePair)
             , ("caseList", PrimCaseList)
             , ("abort", Abort)
             , ("print", Print)
             , ("stop", Stop)
             ]

type TiState = (TiOutput, TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiOutput = [Int]
initialOutput :: TiOutput
initialOutput = []
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
applyToStats f (out, stack, dump, heap, scDefs, stats)
  = (out, stack, dump, heap, scDefs, f stats)

compile :: CoreProgram -> TiState
compile program = (initialOutput, initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
  where
    scDefs = program ++ preludeDefs ++ extraPreludeDefs
    (initialHeap, globals) = buildInitialHeap scDefs
    initialStack = fromList [addressOfMain]
    addressOfMain = aLookup globals "main" (error "main is not defined")

extraPreludeDefs :: CoreProgram
extraPreludeDefs = [ ("False", [], EConstr 1 0)
                   , ("True",  [], EConstr 2 0)
                   , ("not", ["x"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "False")) (EVar "True"))
                   , ("and", ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "y")) (EVar "False"))
                   , ("or",  ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "True")) (EVar "y"))
                   , ("xor", ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EAp (EVar "not") (EVar "y"))) (EVar "y"))
                   , ("MkPair", [], EConstr 1 2)
                   , ("fst", ["p"], EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K"))
                   , ("snd", ["p"], EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K1"))
                   , ("Nil", [], EConstr 1 0)
                   , ("Cons", [], EConstr 2 2)
                   , ("head", ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs")) (EVar "abort")) (EVar "K"))
                   , ("tail", ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs")) (EVar "abort")) (EVar "K1"))
                   , ("printList", ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs")) (EVar "stop")) (EVar "printCons"))
                   , ("printCons", ["h", "t"], EAp (EAp (EVar "print") (EVar "h")) (EAp (EVar "printList") (EVar "t")))
                   ]

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
tiFinal (_, stack, dump, heap, _, _) = isEmpty stack && isEmpty dump

isDataNode :: Node -> Bool
isDataNode (NNum _)    = True
isDataNode (NData _ _) = True
isDataNode _           = False

isIndNode :: Node -> Bool
isIndNode (NInd _) = True
isIndNode _        = False

step :: TiState -> TiState
step state@(output, stack, dump, heap, globals, stats) = dispatch (hLookup heap item)
  where
    (item, stack') = pop stack
    dispatch (NNum n)                  = numStep state n
    dispatch (NAp a1 a2)               = apStep state a1 a2
    dispatch (NSupercomb sc args body) = doAdminSc $ scStep state sc args body
    dispatch (NInd a)                  = indStep state a
    dispatch (NPrim name prim)         = doAdminPrim $ primStep state prim
    dispatch (NData tag fields)        = dataStep state tag fields

numStep :: TiState -> Int -> TiState
numStep (output, stack, dump, heap, globals, stats) n
  | isEmpty stack = error "numStep: empty stack."
  | otherwise     = (output, stack', dump', heap, globals, stats)
  where (stack', dump') = pop dump

apStep :: TiState -> Addr -> Addr -> TiState
apStep (output, stack, dump, heap, globals, stats) a1 a2
  | isIndNode a2node = (output, stack,  dump, heap', globals, stats)
  | otherwise        = (output, stack', dump, heap,  globals, stats)
  where stack' = push a1 stack
        (a0, _) = pop stack
        a2node = hLookup heap a2
        NInd a3 = a2node
        heap' = hUpdate heap a0 (NAp a1 a3)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (output, stack, dump, heap, globals, stats) scName argNames body
  | getDepth stack < length argNames + 1 = error "Too few arguments given"
  | otherwise = (output, stack', dump, heap', globals, stats)
  where
    argsLen = length argNames
    stack' = discard argsLen stack
    (root, _) = pop stack'
    heap' = instantiateAndUpdate body root heap (bindings ++ globals)
    bindings = zip argNames (getargs heap stack)

indStep :: TiState -> Addr -> TiState
indStep (output, stack, dump, heap, globals, stats) a = (output, stack', dump, heap, globals, stats)
  where stack' = push a (discard 1 stack)

primStep :: TiState -> Primitive -> TiState
primStep state Neg                    = primNeg state
primStep state Add                    = primArith state (+)
primStep state Sub                    = primArith state (-)
primStep state Mul                    = primArith state (*)
primStep state Div                    = primArith state div
primStep state (PrimConstr tag arity) = primConstr state tag arity
primStep state If                     = primIf state
primStep state Eq                     = primComp state (==)
primStep state NotEq                  = primComp state (/=)
primStep state Less                   = primComp state (<)
primStep state LessEq                 = primComp state (<=)
primStep state Greater                = primComp state (>)
primStep state GreaterEq              = primComp state (>=)
primStep state PrimCasePair           = primCasePair state
primStep state PrimCaseList           = primCaseList state
primStep state Abort                  = primAbort state
primStep state Print                  = primPrint state
primStep state Stop                   = primStop state

primNeg :: TiState -> TiState
primNeg (output, stack, dump, heap, globals, stats)
  | length args /= 1 = error "primNeg: wrong number of args."
  | isDataNode argnode = (output, stack', dump, heap', globals, stats)
  | otherwise = (output, push argaddr emptyStack, push stack dump, heap, globals, stats)
  where args = getargs heap stack
        [argaddr] = args
        argnode = hLookup heap argaddr
        NNum n = argnode
        (_, stack') = pop stack
        (root, _)   = pop stack'
        heap' = hUpdate heap root (NNum (negate n))

primArith :: TiState -> (Int -> Int -> Int) -> TiState
primArith (output, stack, dump, heap, globals, stats) op
  | length args /= 2 = error "primArith: wrong number of args."
  | not (isDataNode lnode) = (output, push laddr emptyStack, dump', heap, globals, stats)
  | not (isDataNode rnode) = (output, push raddr emptyStack, dump', heap, globals, stats)
  | otherwise = (output, stack', dump, heap', globals, stats)
  where args = getargs heap stack
        [laddr, raddr] = args
        [lnode, rnode] = map (hLookup heap) args
        (NNum m, NNum n) = (lnode, rnode)
        stack' = discard 2 stack
        dump' = push stack' dump
        (root, _) = pop stack'
        heap' = hUpdate heap root (NNum (m `op` n))

primConstr :: TiState -> Tag -> Arity -> TiState
primConstr (output, stack, dump, heap, globals, stats) tag arity
  | length args /= arity = error "primConstr: wrong number of args."
  | otherwise            = (output, stack', dump, heap', globals, stats)
  where args = getargs heap stack
        stack' = discard arity stack
        (root, _) = pop stack'
        heap' = hUpdate heap root (NData tag args)

primIf :: TiState -> TiState
primIf  (output, stack, dump, heap, globals, stats)
  | length args < 3           = error "primIf: wrong number of args."
  | not (isDataNode arg1Node) = (output, push arg1Addr emptyStack, push stack' dump, heap, globals, stats)
  | otherwise                 = (output, stack', dump, heap', globals, stats)
  where args = getargs heap stack
        [arg1Addr, arg2Addr, arg3Addr] = take 3 args
        arg1Node = hLookup heap arg1Addr
        stack' = discard 3 stack
        (root, _) = pop stack'
        result = case arg1Node of
          NData 2 [] -> arg2Addr -- True  case
          NData 1 [] -> arg3Addr -- False case
          _          -> error "primIf: unexpected node found"
        heap' = hUpdate heap root (NInd result)

primComp :: TiState -> (Int -> Int -> Bool) -> TiState
primComp state op = primDyadic state op'
  where op' (NNum m) (NNum n)
          | m `op` n  = NData 2 [] -- True case
          | otherwise = NData 1 [] -- False case
        op' _ _ = error "TODO: implement primComp"

primDyadic :: TiState -> (Node -> Node -> Node) -> TiState
primDyadic (output, stack, dump, heap, globals, stats) op
  | length args /= 2 = error "primDyadic: wrong number of args"
  | not (isDataNode arg1Node) = (output, push arg1Addr emptyStack, dump', heap, globals, stats)
  | not (isDataNode arg2Node) = (output, push arg2Addr emptyStack, dump', heap, globals, stats)
  | otherwise = (output, stack', dump, heap', globals, stats)
  where args = getargs heap stack
        [arg1Addr, arg2Addr] = args
        [arg1Node, arg2Node] = map (hLookup heap) args
        stack' = discard 2 stack
        dump' = push stack' dump
        (root, _) = pop stack'
        heap' = hUpdate heap root (arg1Node `op` arg2Node)

primCasePair :: TiState -> TiState
primCasePair (output, stack, dump, heap, globals, stats)
  | length args /= 2          = error "primCasePair: wrong number of args."
  | not (isDataNode arg1Node) = (output, push arg1Addr emptyStack, push stack' dump, heap, globals, stats)
  | otherwise                 = (output, stack', dump, heap', globals, stats)
  where args = getargs heap stack
        [arg1Addr, arg2Addr] = take 2 args
        arg1Node = hLookup heap arg1Addr
        stack' = discard 2 stack
        (root, _) = pop stack'
        heap' = case arg1Node of
          NData _ [fstAddr, sndAddr] ->
            case hAlloc heap (NAp arg2Addr fstAddr) of
              (heap1, addr) -> hUpdate heap1 root (NAp addr sndAddr)
          _ -> error "primCasePair: invalid node."

primCaseList :: TiState -> TiState
primCaseList (output, stack, dump, heap, globals, stats)
  | length args /= 3          = error "primCaseList: wrong number of args."
  | not (isDataNode arg1Node) = (output, push arg1Addr emptyStack, push stack' dump, heap, globals, stats)
  | otherwise                 = (output, stack', dump, heap', globals, stats)
  where args = getargs heap stack
        [arg1Addr, arg2Addr, arg3Addr] = take 3 args
        arg1Node = hLookup heap arg1Addr
        stack' = discard 3 stack
        (root, _) = pop stack'
        heap' = case arg1Node of
          NData 1 []               -- Nil case
            -> hUpdate heap root (NInd arg2Addr)
          NData 2 [hdAddr, tlAddr] -- Cons case
            -> case hAlloc heap (NAp arg3Addr hdAddr) of
            (heap1, addr) -> hUpdate heap1 root (NAp addr tlAddr)
          _ -> error "primCaseList: Unknown constructor."

primAbort :: TiState -> TiState
primAbort _ = error "abort program."

primPrint :: TiState -> TiState
primPrint (output, stack, dump, heap, globals, stats)
  | length args /= 2   = error "primPrint: wrong number of args."
  | not (isEmpty dump) = error "primPrint: dump isn't empty."
  | otherwise          = case arg1Node of
      NNum n    -> (output++[n], push arg2Addr emptyStack, dump, heap, globals, stats)
      NData _ _ -> error "primPrint: node is not a number."
      _         -> (output, push arg1Addr emptyStack, push stack' dump, heap, globals, stats)
  where args = getargs heap stack
        [arg1Addr, arg2Addr] = take 2 args
        arg1Node = hLookup heap arg1Addr
        stack' = discard 2 stack

primStop :: TiState -> TiState
primStop (output, stack, dump, heap, globals, stats)
  | not (isEmpty dump) = error "primStop: dump isn't empty."
  | otherwise          = (output, stack', dump, heap, globals, stats)
  where (_, stack') = pop stack

dataStep :: TiState -> Tag -> [Addr] -> TiState
dataStep (output, stack, dump, heap, globals, stats) tag fields = (output, stack', dump', heap, globals, stats)
  where (stack', dump') = pop dump

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
instantiateAndUpdate (EConstr tag arity)    updAddr heap env = instantiateAndUpdateConstr tag arity updAddr heap env
instantiateAndUpdate _                      updAddr heap env = error "not yet implemented"

instantiateAndUpdateConstr :: Tag -> Arity -> Addr -> TiHeap -> Assoc Name Addr -> TiHeap
instantiateAndUpdateConstr tag arity updAddr heap env
  = hUpdate heap updAddr (NPrim "Constr" (PrimConstr tag arity))

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

showOutput :: TiOutput -> IseqRep
showOutput output = iStr ("Output " ++ show output)

showDumpMaxDepth :: TiState -> IseqRep
showDumpMaxDepth (_, _, dump, _, _, _)
  = iConcat [ iNewline, iStr "   Dump maximum depth = "
            , iNum (getHighWaterMark dump)
            ]

showStackMaxDepth :: TiState -> IseqRep
showStackMaxDepth (_, stack, _, _, _, _)
  = iConcat [ iNewline, iStr "  Stack maximum depth = "
            , iNum (getHighWaterMark stack)
            ]

showAllocCount :: TiState -> IseqRep
showAllocCount (_, _, _, (allocs, _, _, _), _, _)
  = iConcat [ iNewline, iStr "     Allocation count = "
            , iNum allocs
            ]

showState :: TiState -> IseqRep
showState (output, stack, dump, heap, globals, stats)
  = iConcat [ showHeap heap, iNewline
            , showStack heap stack, iNewline
            , showDumpDepth dump, iNewline
            , showOutput output, iNewline
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
showNode (NData tag fields)    = iConcat [ iStr "NData ", iNum tag, iStr " [", iFields, iStr "]"]
  where iFields = iInterleave (iStr ",") (map showAddr fields)

showAddr :: Addr -> IseqRep
showAddr addr = iStr (showaddr addr)

showFWAddr :: Addr -> IseqRep
showFWAddr addr = iStr (space (4 - length str) ++ str)
  where str = show addr

showStats :: TiState -> IseqRep
showStats (output, stack, dump, heap, globals, stats)
  = iConcat [ iNewline
            , iNewline, iStr "Total number of steps = "
            , iNum (tiStatGetSteps stats)
            , iNewline, iStr "Supercombinator steps = "
            , iNum (tiStatGetScSteps stats)
            , iNewline, iStr "      Primitive steps = "
            , iNum (tiStatGetPrimSteps stats)
            ]
