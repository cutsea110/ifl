module Template.Mark5
  ( parse
  , eval
  , compile
  , cnv
  , showResults
  , runProg
  , runProgWithConv
  ) where

import Data.List (mapAccumL)

import Iseq
import Language
import Heap
import Stack
import Utils


runProg :: String -> String
runProg = showResults . eval . compile . parse

runProgWithConv :: String -> String
runProgWithConv = showResults . eval . cnv . compile . parse

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

data TiState
  = TiState { tiOutput  :: TiOutput
            , tiStack   :: TiStack
            , tiDump    :: TiDump
            , tiHeap    :: TiHeap
            , tiGlobals :: TiGlobals
            , tiStats   :: TiStats
            }

type TiOutput = [Int]
initialOutput :: TiOutput
initialOutput = []
type TiStack = Stack Addr
type TiDump = Stack TiStack
initialTiDump :: TiDump
initialTiDump = emptyStack
type TiHeap = Heap Node
type TiGlobals = Assoc Name Addr
data TiStats = TiStats { stepTotal :: Int  -- ^ total steps
                       , stepSc    :: Int  -- ^ super combinator steps
                       , stepPrim  :: Int  -- ^ primitive steps
                       }
tiStatInitial :: TiStats
tiStatInitial = TiStats { stepTotal = 0, stepSc = 0, stepPrim = 0 }
tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps s = s { stepTotal = stepTotal s + 1 }
tiStatIncScSteps :: TiStats -> TiStats
tiStatIncScSteps s = s { stepSc = stepSc s + 1 }
tiStatIncPrimSteps :: TiStats -> TiStats
tiStatIncPrimSteps s = s { stepPrim = stepPrim s + 1 }
tiStatGetSteps :: TiStats -> Int
tiStatGetSteps s = stepTotal s
tiStatGetScSteps :: TiStats -> Int
tiStatGetScSteps s = stepSc s
tiStatGetPrimSteps :: TiStats -> Int
tiStatGetPrimSteps s = stepPrim s
applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f state@(TiState _ _ _ _ _ stats)
  = state { tiStats = f stats }

compile :: CoreProgram -> TiState
compile program = TiState initialOutput initialStack initialTiDump initialHeap1 initialGlobals tiStatInitial
  where
    scDefs = program ++ preludeDefs ++ extraPreludeDefs
    (initialHeap, initialGlobals) = buildInitialHeap scDefs
    initialStack = push addr emptyStack
    addressOfMain = aLookup initialGlobals "main" (error "main is not defined")
    addressOfPrint = aLookup initialGlobals "printList" (error "printList is not defined")
    (initialHeap1, addr) = hAlloc initialHeap (NAp addressOfPrint addressOfMain)

-- | convert newer version
-- main := Cons main Nil
cnv :: TiState -> TiState
cnv state@(TiState _ _ _ heap globals _) = state { tiStack = initialStack, tiHeap = initialHeap }
  where
    initialStack = push addr emptyStack
    addressOfMain = aLookup globals "main" (error "main is not defined")
    addressOfPrint = aLookup globals "printList" (error "printList is not defined")
    addressOfCons = aLookup globals "Cons" (error "Nil is not defined")
    addressOfNil = aLookup globals "Nil" (error "Nil is not defined")
    (heap1, addr1) = hAlloc heap (NAp addressOfCons addressOfMain)
    (heap2, addr2) = hAlloc heap1 (NAp addr1 addressOfNil)
    (initialHeap, addr) = hAlloc heap2 (NAp addressOfPrint addr2)
    

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
tiFinal (TiState _ stack dump _ _ _) = isEmpty stack && isEmpty dump

isDataNode :: Node -> Bool
isDataNode (NNum _)    = True
isDataNode (NData _ _) = True
isDataNode _           = False

isIndNode :: Node -> Bool
isIndNode (NInd _) = True
isIndNode _        = False

step :: TiState -> TiState
step state@(TiState _ stack _ heap _ _) = dispatch (hLookup heap item)
  where
    (item, _) = pop stack
    dispatch (NNum n)                  = numStep n state
    dispatch (NAp a1 a2)               = apStep a1 a2 state
    dispatch (NSupercomb sc args body) = doAdminSc $ scStep sc args body state
    dispatch (NInd a)                  = indStep a state
    dispatch (NPrim _ prim)            = primStep prim state
    dispatch (NData tag fields)        = dataStep tag fields state

numStep :: Int -> TiState -> TiState
numStep _ state@(TiState _ stack dump _ _ _)
  | isEmpty stack = error "numStep: empty stack."
  | otherwise     = state { tiStack = stack', tiDump = dump' }
  where (stack', dump') = restore stack dump

apStep :: Addr -> Addr -> TiState ->  TiState
apStep a1 a2 state@(TiState _ stack _ heap _ _)
  | isIndNode a2node = state { tiHeap = heap' }
  | otherwise        = state { tiStack = stack' }
  where stack' = push a1 stack
        (a0, _) = pop stack
        a2node = hLookup heap a2
        NInd a3 = a2node
        heap' = hUpdate heap a0 (NAp a1 a3)

scStep :: Name -> [Name] -> CoreExpr -> TiState -> TiState
scStep _ argNames body state@(TiState _ stack _ heap globals _)
  | getDepth stack < length argNames + 1 = error "Too few arguments given"
  | otherwise = state { tiStack = stack', tiHeap = heap' }
  where
    argsLen = length argNames
    stack' = discard argsLen stack
    (root, _) = pop stack'
    heap' = instantiateAndUpdate body root heap (bindings ++ globals)
    bindings = zip argNames (getargs heap stack)

indStep :: Addr -> TiState -> TiState
indStep a state@(TiState _ stack _ _ _ _) = state { tiStack = stack' }
  where stack' = push a (discard 1 stack)

primStep :: Primitive -> TiState -> TiState
primStep Neg                    state = primNeg state
primStep Add                    state = primArith (+) state
primStep Sub                    state = primArith (-) state
primStep Mul                    state = primArith (*) state
primStep Div                    state = primArith div state
primStep (PrimConstr tag arity) state = primConstr tag arity state
primStep If                     state = primIf state
primStep Eq                     state = primComp (==) state
primStep NotEq                  state = primComp (/=) state
primStep Less                   state = primComp (<) state
primStep LessEq                 state = primComp (<=) state
primStep Greater                state = primComp (>) state
primStep GreaterEq              state = primComp (>=) state
primStep PrimCasePair           state = primCasePair state
primStep PrimCaseList           state = primCaseList state
primStep Abort                  state = primAbort state
primStep Print                  state = primPrint state
primStep Stop                   state = primStop state

primNeg :: TiState -> TiState
primNeg state@(TiState _ stack dump heap _ _)
  | length args /= 1 = error "primNeg: wrong number of args."
  | isDataNode argnode = doAdminPrim state { tiStack = stack', tiHeap = heap' }
  | otherwise = state { tiStack = push argaddr emptyStack, tiDump = push stack dump }
  where args = getargs heap stack
        [argaddr] = args
        argnode = hLookup heap argaddr
        NNum n = argnode
        (_, stack') = pop stack
        (root, _)   = pop stack'
        heap' = hUpdate heap root (NNum (negate n))

primArith :: (Int -> Int -> Int) -> TiState -> TiState
primArith op state@(TiState _ stack dump heap _ _)
  | length args /= 2 = error "primArith: wrong number of args."
  | not (isDataNode lnode) = state { tiStack = push laddr emptyStack, tiDump = dump' }
  | not (isDataNode rnode) = state { tiStack = push raddr emptyStack, tiDump = dump' }
  | otherwise = doAdminPrim state { tiStack = stack',tiHeap = heap' }
  where args = getargs heap stack
        [laddr, raddr] = args
        [lnode, rnode] = map (hLookup heap) args
        (NNum m, NNum n) = (lnode, rnode)
        stack' = discard 2 stack
        dump' = push stack' dump
        (root, _) = pop stack'
        heap' = hUpdate heap root (NNum (m `op` n))

primConstr :: Tag -> Arity -> TiState -> TiState
primConstr tag arity state@(TiState _ stack _ heap _ _)
  | length args /= arity = error "primConstr: wrong number of args."
  | otherwise            = doAdminPrim state { tiStack = stack', tiHeap = heap' }
  where args = getargs heap stack
        stack' = discard arity stack
        (root, _) = pop stack'
        heap' = hUpdate heap root (NData tag args)

primIf :: TiState -> TiState
primIf state@(TiState _ stack dump heap _ _)
  | length args < 3           = error "primIf: wrong number of args."
  | not (isDataNode arg1Node) = state { tiStack = push arg1Addr emptyStack, tiDump = push stack' dump }
  | otherwise                 = doAdminPrim state { tiStack = stack', tiHeap = heap' }
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

primComp :: (Int -> Int -> Bool) -> TiState -> TiState
primComp op state = doAdminPrim $ primDyadic op' state
  where op' (NNum m) (NNum n)
          | m `op` n  = NData 2 [] -- True case
          | otherwise = NData 1 [] -- False case
        op' _ _ = error "TODO: implement primComp"

primDyadic :: (Node -> Node -> Node) -> TiState -> TiState
primDyadic op state@(TiState _ stack dump heap _ _)
  | length args /= 2 = error "primDyadic: wrong number of args"
  | not (isDataNode arg1Node) = state { tiStack = push arg1Addr emptyStack, tiDump = dump' }
  | not (isDataNode arg2Node) = state { tiStack = push arg2Addr emptyStack, tiDump = dump' }
  | otherwise = doAdminPrim state { tiStack = stack', tiHeap = heap' }
  where args = getargs heap stack
        [arg1Addr, arg2Addr] = args
        [arg1Node, arg2Node] = map (hLookup heap) args
        stack' = discard 2 stack
        dump' = push stack' dump
        (root, _) = pop stack'
        heap' = hUpdate heap root (arg1Node `op` arg2Node)

primCasePair :: TiState -> TiState
primCasePair state@(TiState _ stack dump heap _ _)
  | length args /= 2          = error "primCasePair: wrong number of args."
  | not (isDataNode arg1Node) = state { tiStack = push arg1Addr emptyStack, tiDump = push stack' dump }
  | otherwise                 = doAdminPrim state { tiStack = stack', tiHeap = heap' }
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
primCaseList state@(TiState _ stack dump heap _ _)
  | length args /= 3          = error "primCaseList: wrong number of args."
  | not (isDataNode arg1Node) = state { tiStack = push arg1Addr emptyStack, tiDump = push stack' dump }
  | otherwise                 = doAdminPrim state { tiStack = stack', tiHeap = heap' }
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
primAbort _ = error "abort program." -- WANTFIX: doAdminPrim?

primPrint :: TiState -> TiState
primPrint state@(TiState output stack dump heap _ _)
  | length args /= 2   = error "primPrint: wrong number of args."
  | not (isEmpty dump) = error "primPrint: dump isn't empty."
  | otherwise          = doAdminPrim $ case arg1Node of
      NNum n    -> state { tiOutput = output++[n], tiStack = push arg2Addr emptyStack }
      NData _ _ -> error "primPrint: node is not a number."
      _         -> state { tiStack = push arg1Addr emptyStack, tiDump = push stack' dump }
  where args = getargs heap stack
        [arg1Addr, arg2Addr] = take 2 args
        arg1Node = hLookup heap arg1Addr
        stack' = discard 2 stack

primStop :: TiState -> TiState
primStop state@(TiState _ stack dump _ _ _)
  | not (isEmpty dump) = error "primStop: dump isn't empty."
  | otherwise          = doAdminPrim state { tiStack = stack' }
  where (_, stack') = pop stack

dataStep :: Tag -> [Addr] -> TiState -> TiState
dataStep _ _ state@(TiState _ stack dump _ _ _) = state { tiStack = stack', tiDump = dump' }
  where (stack', dump') = restore stack dump

instantiateAndUpdate :: CoreExpr -> Addr -> TiHeap -> Assoc Name Addr -> TiHeap
instantiateAndUpdate (ENum n)               updAddr heap _   = hUpdate heap updAddr (NNum n)
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
        instantiateRhs hp (name, rhs) = (hp', (name, addr))
          where (hp', addr) = instantiate rhs hp rhsEnv
instantiateAndUpdate (EConstr tag arity)    updAddr heap env = instantiateAndUpdateConstr tag arity updAddr heap env
instantiateAndUpdate _                      _       _    _   = error "not yet implemented"

instantiateAndUpdateConstr :: Tag -> Arity -> Addr -> TiHeap -> Assoc Name Addr -> TiHeap
instantiateAndUpdateConstr tag arity updAddr heap _
  = hUpdate heap updAddr (NPrim "Constr" (PrimConstr tag arity))

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap stack = case getStack stack of
  []       -> error "Empty stack"
  _:stack' -> map getarg stack'
    where getarg addr = arg
            where NAp _fun arg = hLookup heap addr

instantiate :: CoreExpr -> TiHeap -> Assoc Name Addr -> (TiHeap, Addr)
instantiate (ENum n)               heap _   = hAlloc heap (NNum n)
instantiate (EAp e1 e2)            heap env = hAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap  env
    (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v)               heap env = (heap, aLookup env v (error ("Undefined name " ++ show v)))
instantiate (EConstr tag arity)    heap env = instantiateConstr tag arity heap env
instantiate (ELet isrec defs body) heap env = instantiateLet isrec defs body heap env
instantiate (ECase _ _)            _    _   = error "Can't instantiate case exprs"
instantiate (ELam _ _)             _    _   = error "Can't instantiate lambda abstractions"

instantiateConstr :: Tag -> Arity -> TiHeap -> Assoc Name Addr -> (TiHeap, Addr)
instantiateConstr = error "TODO: implement instantiateConstr"

instantiateLet :: IsRec -> [(Name, CoreExpr)] -> CoreExpr -> TiHeap -> Assoc Name Addr -> (TiHeap, Addr)
instantiateLet isrec defs expr heap env = instantiate expr heap' env'
  where
    (heap', extraBindings) = mapAccumL instantiateRhs heap defs
    env' = extraBindings ++ env
    rhsEnv | isrec     = env'
           | otherwise = env
    instantiateRhs hp (name, rhs) = (heap1, (name, addr))
      where (heap1, addr) = instantiate rhs hp rhsEnv

------

showResults :: [TiState] -> String
showResults states
  = unlines (map iDisplay (iLayn' (map showState states) ++
             [ showStats lastState
             , showAllocCount lastState
             , showStackMaxDepth lastState
             , showDumpMaxDepth lastState
             ]))
  where
    lastState = last states

showOutput :: TiOutput -> IseqRep
showOutput output = iStr ("Output " ++ show output)

showDumpMaxDepth :: TiState -> IseqRep
showDumpMaxDepth (TiState _ _ dump _ _ _)
  = iConcat [ iStr "   Dump maximum depth = "
            , iNum (getHighWaterMark dump)
            ]

showStackMaxDepth :: TiState -> IseqRep
showStackMaxDepth (TiState _ stack _ _ _ _)
  = iConcat [ iStr "  Stack maximum depth = "
            , iNum (getHighWaterMark stack)
            ]

showAllocCount :: TiState -> IseqRep
showAllocCount (TiState _ _ _ (allocs, _, _, _) _ _)
  = iConcat [ iStr "     Allocation count = "
            , iNum allocs
            ]

showState :: TiState -> IseqRep
showState (TiState output stack dump heap _ _)
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
showStkNode _ node = showNode node

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
showStats (TiState _ _ _ _ _ stats)
  = iConcat [ iNewline
            , iNewline, iStr "Total number of steps = "
            , iNum (tiStatGetSteps stats)
            , iNewline, iStr "Supercombinator steps = "
            , iNum (tiStatGetScSteps stats)
            , iNewline, iStr "      Primitive steps = "
            , iNum (tiStatGetPrimSteps stats)
            ]
