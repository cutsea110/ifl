{-# LANGUAGE NPlusKPatterns  #-}
module Gmachine.Mark7
  ( parse
  , compile
  , eval
  , showResults
  , runProg
  ) where

import Heap
import Iseq
import Language
import qualified Parser as P (runParser)
import qualified Stack as S
import Utils
import Data.Char (chr, ord)
import Data.List (mapAccumL, (\\))

runProg :: Bool -> String -> String
runProg verbose = showR . eval . compile . parse
  where showR | verbose   = showResults
              | otherwise = showSimpleResult

data GmState = GmState { output  :: GmOutput
                       , code    :: GmCode
                       , stack   :: GmStack
                       , vstack  :: GmVStack
                       , dump    :: GmDump
                       , heap    :: GmHeap
                       , globals :: GmGlobals
                       , stats   :: GmStats
                       }

type GmOutput = ([String], IseqRep)
initialOutput :: GmOutput
initialOutput = ([], iNil)

pushOutput :: String -> GmOutput -> GmOutput
pushOutput s (o, _) = (o ++ [s], iStr s)
outputAll :: GmOutput -> String
outputAll (o, _) = concat o
outputLast :: GmOutput -> IseqRep
outputLast (_, s) = s
clearOutputLast :: GmOutput -> GmOutput
clearOutputLast (o, _) = (o, iNil)

getOutput :: GmState -> GmOutput
getOutput state = output state

putOutput :: GmOutput -> GmState -> GmState
putOutput o state = state { output = o }


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
  | Push Int   -- push offset
  | Pop Int    -- pop offset
  | Update Int -- update offset by stack top
  | Slide Int
  | Alloc Int
  | Eval
  | Add | Sub | Mul | Div | Neg
  | Eq | Ne | Lt | Le | Gt | Ge
  | Cond GmCode GmCode
  | Pack Tag Arity
  | Casejump [(Tag, GmCode)]
  | Split Arity
  | Pushbasic Int
  | UpdateBool Int
  | UpdateInt Int
  | Get
  | Return
  | Print
  | PutChar
  deriving (Eq, Show)

type GmStack = S.Stack Addr

getStack :: GmState -> GmStack
getStack state = stack state

putStack :: GmStack -> GmState -> GmState
putStack stack' state = state { stack = stack' }

type GmVStack = S.Stack Int

getVStack :: GmState -> GmVStack
getVStack state = vstack state

putVStack :: GmVStack -> GmState -> GmState
putVStack vstack' state = state { vstack = vstack' }

type GmDump = S.Stack GmDumpItem
type GmDumpItem = (GmCode, GmStack, GmVStack)

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
  | NConstr Int [Addr]
  deriving Show

instance Eq Node where
  NNum a == NNum b = a == b
  _      == _      = error "undefined comparison"

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
step state = case code of
  [] -> error "no code"
  (i:is) -> dispatch i
            . putOutput (clearOutputLast o)
            . putCode is
            $ state
  where code = getCode state
        o = getOutput state

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
dispatch (Cond t f)     = cond t f
dispatch (Pack t n)     = pack t n
dispatch (Casejump bs)  = casejump bs
dispatch (Split n)      = split n
dispatch (Pushbasic n)  = pushbasic n
dispatch (UpdateBool n) = updatebool n
dispatch (UpdateInt n)  = updateint n
dispatch Get            = gmget
dispatch Return         = gmreturn
dispatch Print          = gmprint
dispatch PutChar        = putchar

evalop :: GmState -> GmState
evalop state = newState (hLookup h a)
  where newState e = case e of
          (NNum _) -> state
          (NConstr _ _) -> state
          _ -> putCode    [Unwind]
               . putStack (S.push a S.emptyStack)
               . putVStack S.emptyStack
               . putDump  (S.push (i, s, v) d)
               $ state
        h = getHeap state
        d = getDump state
        v = getVStack state
        (a, s) = S.pop (getStack state)
        i = getCode state

arithmetic1 :: (Int -> Int)         -- arithmetic operator
            -> (GmState -> GmState) -- state transition
arithmetic1 op state = putVStack vstack'' state
  where vstack = getVStack state
        vstack'' = S.push (op n) vstack'
        (n, vstack') = S.pop vstack
    
arithmetic2 :: (Int -> Int -> Int)  -- arithmetic operator
            -> (GmState -> GmState) -- state transition
arithmetic2 op state = putVStack vstack'' state
  where vstack = getVStack state
        vstack'' = S.push (op n0 n1) vstack'
        ((n0:n1:_), vstack') = S.nPop 2 vstack

comparison :: (Int -> Int -> Bool) -> GmState -> GmState
comparison op state = putVStack vstack'' state
  where vstack = getVStack state
        vstack'' = S.push b vstack'
        ((n0:n1:_), vstack') = S.nPop 2 vstack
        b | op n0 n1  = 2 -- True
          | otherwise = 1 -- False

cond :: GmCode -> GmCode -> GmState -> GmState
cond i1 i2 state = putVStack vstack'
                   . putCode i'
                   $ state
  where vstack = getVStack state
        (b, vstack') = S.pop vstack
        i = getCode state
        i' = case b of
          2 -> (i1 ++ i) -- True
          1 -> (i2 ++ i) -- False
          e -> error $ "not integer which can be regarded as boolean: " ++ show e

pack :: Tag -> Arity -> GmState -> GmState
pack t n state = putStack (S.push a s')
                 . putHeap h'
                 $ state
  where (as, s') = S.nPop n (getStack state)
        d = NConstr t as
        h = getHeap state
        (h', a) = hAlloc h d

casejump :: [(Tag, GmCode)] -> GmState -> GmState
casejump bs state = putCode (i' ++ i) state
  where (a, _) = S.pop (getStack state)
        i = getCode state
        h = getHeap state
        i' = newState (hLookup h a)
        newState e = case e of
          NConstr t _
            -> aLookup bs t (error "unknown tag")
          _ -> error $ "not data structure: " ++ show e

split :: Int -> GmState -> GmState
split n state = putStack s'' state
  where (a, s') = S.pop (getStack state)
        h = getHeap state
        s'' = newState (hLookup h a)
        newState e = case e of
          NConstr t as
            | length as == n -> foldr S.push s' as
            | otherwise -> error "non-saturated"
          _ -> error $ "not data structure: " ++ show e

pushbasic :: Int -> GmState -> GmState
pushbasic i state = putVStack (S.push i vstack) state
  where vstack = getVStack state

mkbool :: GmState -> GmState
mkbool state = putStack stack'
               . putHeap heap'
               . putVStack vstack'
               $ state
  where stack = getStack state
        vstack = getVStack state
        heap = getHeap state
        (t, vstack') = S.pop vstack
        (heap', a) = hAlloc heap (NConstr t [])
        stack' = S.push a stack

mkint :: GmState -> GmState
mkint state = putStack stack'
              . putHeap heap'
              . putVStack vstack'
              $ state
  where stack = getStack state
        vstack = getVStack state
        heap = getHeap state
        (n, vstack') = S.pop vstack
        (heap', a) = hAlloc heap (NNum n)
        stack' = S.push a stack

updatebool :: Int -> GmState -> GmState
updatebool n = sub . mkbool
  where 
    sub state = putHeap heap' (putStack s' state)
      where s = getStack state
            (a, s') = S.pop s
            a' = S.getStack s' !! n
            b = hLookup (getHeap state) a
            heap' = hUpdate (getHeap state) a' b

updateint :: Int -> GmState -> GmState
updateint n = sub . mkint
  where 
    sub state = putHeap heap' (putStack s' state)
      where s = getStack state
            (a, s') = S.pop s
            a' = S.getStack s' !! n
            i = hLookup (getHeap state) a
            heap' = hUpdate (getHeap state) a' i

gmget :: GmState -> GmState
gmget state = newState (hLookup heap a) (putStack stack' state)
  where stack = getStack state
        vstack = getVStack state
        heap = getHeap state
        (a, stack') = S.pop stack
        newState e = case e of
          NConstr t [] -> putVStack (S.push t vstack)
          NNum n       -> putVStack (S.push n vstack)
          _            -> error $ "Get of a non-number or bool: " ++ show e

gmreturn :: GmState -> GmState
gmreturn state = putCode i
                 . putStack stack'
                 . putVStack v
                 . putDump d
                 $ state
  where stack = getStack state
        dump = getDump state
        ((i, s, v), d) = S.pop dump
        k = S.getDepth stack
        (ak, _) = S.pop (S.discard (k-1) stack)
        stack' = S.push ak s


gmprint :: GmState -> GmState
gmprint state = case hLookup h a of
          NNum n -> putOutput (pushOutput (show n) o)
                    . putStack s
                    $ state
          NConstr t as -> putOutput (pushOutput (lparen ++ showConstr t (length as)) o)
                          . putCode (printcode as ++ rparen ++ i)
                          . putStack (foldr S.push s as)
                          $ state
            where len = length as
                  needParen = len > 0
                  (lparen, rparen) = if not needParen then ("", [])
                                     else ("(", [Pushint (ord ')'), PutChar])
          _ -> error "can not print"
  where (a, s) = S.pop (getStack state)
        h = getHeap state
        o = getOutput state
        i = getCode state
        printcode = foldr (\_ xs -> Pushint (ord ' '):PutChar:Eval:Print:xs) []

putchar :: GmState -> GmState
putchar state = case hLookup h a of
  NNum n -> putOutput (pushOutput [chr n] o)
            . putStack s
            $ state
  _ -> error "can not putchar"
  where (a, s) = S.pop (getStack state)
        h = getHeap state
        o = getOutput state

showConstr :: Tag -> Arity -> String
showConstr t a = "Pack{" ++ show t ++ "," ++ show a ++ "}"

readPack :: String -> Maybe Instruction
readPack s = case res of
  [] -> Nothing
  ((t, a), []):_ -> Just (Pack t a)
  _ -> error "failed to parse for Pack"
  where res = P.runParser pConstr (clex 0 s)


pushglobal :: Name -> GmState -> GmState
pushglobal f state = case readPack f of
  Nothing                      -> putStack (S.push a stack) state

  Just (Pack tag arity)
    | f `elem` aDomain globals -> putStack (S.push a stack) state
    | otherwise                -> putStack (S.push a stack)
                                  . putHeap h'
                                  . putGlobals (aInsert globals (showConstr tag arity) a)
                                  $ state
    where a = aLookup globals f a'
          (h', a') = hAlloc heap gNode
          gNode = NGlobal arity [Pack tag arity, Update 0, Unwind]
  Just _                       -> error "pushglobal: not Pack"
  where stack = getStack state
        heap = getHeap state
        globals = getGlobals state
        a = aLookup globals f (error $ "Undeclared global " ++ f)

pushint :: Int -> GmState -> GmState
pushint n state = case aLookup (getGlobals state) name (-1) of
  a' | a' < 0    -> putStack (S.push a (getStack state))
                    . putHeap heap'
                    . putGlobals (aInsert (getGlobals state) name a')
                    $ state
     | otherwise -> putStack (S.push a' (getStack state)) state
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
alloc n state = putStack stack2
                . putHeap heap2
                $ state
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
        newState (NNum n)
          | S.isEmpty dump = putCode [] state
          | otherwise      = putCode i'
                             . putStack (S.push a s')
                             . putVStack v'
                             . putDump d
                             $ state
        newState (NAp a1 a2) = putCode [Unwind]
                               . putStack (S.push a1 s)
                               $ state
        newState (NGlobal n c)
          | k < n     = putCode i'
                        . putStack (S.push ak s')
                        . putVStack v'
                        . putDump d
                        $ state
          | otherwise = putCode c
                        . putStack (rearrange n heap s)
                        $ state
          where k             = S.getDepth s1
                (ak, _)       = S.pop (S.discard k s)
        newState (NInd a1) = putCode [Unwind]
                             . putStack (S.push a1 s1)
                             $ state
        newState (NConstr _ _)
          | S.isEmpty dump = putCode [] state
          | otherwise      = putCode i'
                             . putStack (S.push a s')
                             . putVStack v'
                             . putDump d
                             $ state
        ((i', s', v'), d) = S.pop dump


compile :: CoreProgram -> GmState
compile program = GmState { output  = initialOutput
                          , code    = initialCode
                          , stack   = S.emptyStack
                          , vstack  = S.emptyStack
                          , dump    = S.emptyStack
                          , heap    = heap
                          , globals = globals
                          , stats   = statInitial
                          }
  where (heap, globals) = buildInitialHeap program

buildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildInitialHeap program = mapAccumL allocateSc hInitial compiled
  where
    compiled = map compileSc (preludeDefs ++ extraPreludeDefs ++ program ++ primitives) ++ compiledPrimitives

extraPreludeDefs :: CoreProgram
extraPreludeDefs = parse extraPreludeCode

extraPreludeCode :: String
extraPreludeCode = unlines extraPrelude

extraPrelude :: [String]
extraPrelude
  = [ "flip f x y = f y x;"
    , "plus x y = x + y;"
    , "divMod x y = let d = x / y in Pair d (x-d*y);"

    , "showBool b = case b of"
    , "  <1> -> Cons 70 (Cons 97 (Cons 108 (Cons 115 (Cons 101 Nil))));"
    , "  <2> -> Cons 84 (Cons 114 (Cons 117 (Cons 101 Nil)));"
    , "and x y = x && y;"
    , "or x y = x || y;"
    , "xor x y = (x || y) && not (x && y);"

    , "Nothing = Pack{0,0};"
    , "Just x = Pack{1,1} x;"
    , "maybe n f x = case x of"
    , "  <0> -> n;"
    , "  <1> a -> f a;"
    , "putMaybe p x = case x of"
    , "  <0>   -> putStr (Cons 78 (Cons 111 (Cons 116 (Cons 104 (Cons 105 (Cons 110 (Cons 103 Nil)))))));"
    , "  <1> x -> seq (putStr (Cons 74 (Cons 117 (Cons 115 (Cons 116 (Cons 32 Nil)))))) (p x);"

    , "Pair l r = Pack{0,2} l r;"
    , "fst p = case p of"
    , "  <0> a b -> a;"
    , "snd p = case p of"
    , "  <0> a b -> b;"
    , "dup x = Pair x x;"
    , "swap p = case p of"
    , "  <0> x y -> Pair y x;"
    , "putPair p1 p2 x = case x of"
    , "  <0> a b -> let tpl = Cons (p1 a) (Cons (putChar 44) (Cons (p2 b) Nil))"
    , "             in foldr seq 0 (bracket (putChar 40) tpl (putChar 41));"

    , "Left  x = Pack{1,1} x;"
    , "Right x = Pack{2,1} x;"
    , "either f g x = case x of"
    , "  <1> a -> f a;"
    , "  <2> b -> g b;"
    , "putEither p1 p2 x = case x of"
    , "  <1> a -> seq (putStr (Cons 76 (Cons 101 (Cons 102 (Cons 116 (Cons 32 Nil)))))) (p1 a);"
    , "  <2> b -> seq (putStr (Cons 82 (Cons 105 (Cons 103 (Cons 104 (Cons 116 (Cons 32 Nil))))))) (p2 b);"

    , "Nil = Pack{1,0};"
    , "Cons x xs = Pack{2,2} x xs;"
    , "null xs = case xs of"
    , "  <1>     -> True;"
    , "  <2> a b -> False;"
    , "foldr f seed xs = case xs of"
    , "  <1>      -> seed;"
    , "  <2> y ys -> f y (foldr f seed ys);"
    , "map f xs = case xs of"
    , "  <1>      -> Nil;"
    , "  <2> y ys -> Cons (f y) (map f ys);"
    , "unfoldr psi xs = case psi xs of"
    , "  <0>   -> Nil;"
    , "  <1> p -> case p of"
    , "      <0> a b -> Cons a (unfoldr psi b);"
    , "append xs ys = foldr Cons ys xs;"
    , "bracket l xs r = Cons l (append xs (Cons r Nil));"
    , "intersperse sep xs = foldr (withSep sep) Nil xs;"
    , "withSep sep a b = if (null b) (Cons a b) (Cons a (Cons sep b));"
    , "pLsub p xs = case xs of"
    , "  <1> -> putChar 93;"
    , "  <2> y ys -> seq (p y) (if (null ys) (putChar 93) (seq (putChar 44) (pLsub p ys)));"
    , "putList p xs = seq (putChar 91) (pLsub p xs);"

    , "seq x y = y + 0 * x;" -- means 'x >> y' but just kidding.
    , "putStr cs = foldr seq 0 (map putChar cs);"
    , "putStrLn cs = foldr seq (putChar 10) (map putChar cs)"
    ]

primitives :: [CoreScDefn]
primitives
  = [ ("+", ["x", "y"], EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))
    , ("-", ["x", "y"], EAp (EAp (EVar "-") (EVar "x")) (EVar "y"))
    , ("*", ["x", "y"], EAp (EAp (EVar "*") (EVar "x")) (EVar "y"))
    , ("/", ["x", "y"], EAp (EAp (EVar "/") (EVar "x")) (EVar "y"))
    , ("negate", ["x"], EAp (EVar "negate") (EVar "x"))
    , ("==", ["x", "y"], EAp (EAp (EVar "==") (EVar "x")) (EVar "y"))
    , ("/=", ["x", "y"], EAp (EAp (EVar "/=") (EVar "x")) (EVar "y"))
    , ("<", ["x", "y"], EAp (EAp (EVar "<") (EVar "x")) (EVar "y"))
    , ("<=", ["x", "y"], EAp (EAp (EVar "<=") (EVar "x")) (EVar "y"))
    , (">", ["x", "y"], EAp (EAp (EVar ">") (EVar "x")) (EVar "y"))
    , (">=", ["x", "y"], EAp (EAp (EVar ">=") (EVar "x")) (EVar "y"))
    , ("&&", ["x", "y"], EAp (EAp (EVar "&&") (EVar "x")) (EVar "y"))
    , ("||", ["x", "y"], EAp (EAp (EVar "||") (EVar "x")) (EVar "y"))
    , ("not", ["x"], EAp (EVar "not") (EVar "x"))

    , ("if", ["c", "t", "f"], EAp (EAp (EAp (EVar "if") (EVar "c")) (EVar "t")) (EVar "f"))
    , ("True", [], EConstr 2 0)
    , ("False", [], EConstr 1 0)
    ]

compiledPrimitives :: [GmCompiledSC]
compiledPrimitives
  = [ ("putNumber", 1, [Push 0, Eval, Print, Unwind])
    , ("putChar", 1, [Push 0, Eval, PutChar, Unwind])
    ]

type GmCompiledSC = (Name, Int, GmCode)

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns) = (heap', (name, addr))
  where (heap', addr) = hAlloc heap (NGlobal nargs instns)

initialCode :: GmCode
initialCode = [Pushglobal "main", Eval, Print]

{- |
>>> compileSc ("K", ["x", "y"], EVar "x")
("K",2,[Push 0,Eval,Update 2,Pop 2,Unwind])

>>> compileSc ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x")))
("S",3,[Push 2,Push 2,Mkap,Push 3,Push 2,Mkap,Mkap,Eval,Update 3,Pop 3,Unwind])

>>> compileSc . head . parse $ "Y f = letrec x = f x in x"
("Y",1,[Alloc 1,Push 0,Push 2,Mkap,Update 0,Push 0,Eval,Update 2,Pop 2,Unwind])

>>> compileSc . head . parse $ "x = 42"
("x",0,[Pushint 42,Update 0,Pop 0,Unwind])

>>> compileSc . head . parse $ "f x y = letrec a = Pack{0,2} x b; b = Pack{0,2} y a in fst (snd (snd a))"
("f",2,[Alloc 2,Push 0,Push 3,Pack 0 2,Update 1,Push 1,Push 4,Pack 0 2,Update 0,Push 1,Pushglobal "snd",Mkap,Pushglobal "snd",Mkap,Pushglobal "fst",Mkap,Eval,Update 4,Pop 4,Unwind])

>>> compileSc . head . parse $ "main = 3+4*5"
("main",0,[Pushbasic 5,Pushbasic 4,Mul,Pushbasic 3,Add,UpdateInt 0,Return])

>>> compileSc . head . parse $ "minus3 = negate 3"
("minus3",0,[Pushbasic 3,Neg,UpdateInt 0,Return])

>>> compileSc . head . parse $ "f b = not b"
("f",1,[Push 0,Eval,Get,Cond [Pushbasic 1] [Pushbasic 2],UpdateBool 1,Return])

>>> compileSc . head . parse $ "and x y = x && y"
("and",2,[Push 0,Eval,Get,Cond [Push 1,Eval,Get] [Pushbasic 1],UpdateBool 2,Return])

>>> compileSc . head . parse $ "or x y = x || y"
("or",2,[Push 0,Eval,Get,Cond [Pushbasic 2] [Push 1,Eval,Get],UpdateBool 2,Return])

>>> compileSc . head . parse $ "xor x y = (x || y) && not (x && y)"
("xor",2,[Push 0,Eval,Get,Cond [Pushbasic 2] [Push 1,Eval,Get],Cond [Push 0,Eval,Get,Cond [Push 1,Eval,Get] [Pushbasic 1],Cond [Pushbasic 1] [Pushbasic 2]] [Pushbasic 1],UpdateBool 2,Return])

>>> compileSc . head . parse $ "fac n = if (n == 0) 1 (n * fac (n-1))"
("fac",1,[Pushbasic 0,Push 0,Eval,Get,Eq,Cond [Pushint 1,Update 1,Pop 1,Unwind] [Pushint 1,Push 1,Pushglobal "-",Mkap,Mkap,Pushglobal "fac",Mkap,Eval,Get,Push 0,Eval,Get,Mul,UpdateInt 1,Return]])
-}
compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC
compileSc (name, env, body) = (name, length env, compileR body (zip env [0..]))

-- maybe Reduction's R
compileR :: GmCompiler
compileR e env = case e of
  ELet recursive defs e
    | recursive -> compileLetrecR compileR defs e env
    | otherwise -> compileLetR    compileR defs e env
  EAp (EVar "negate") _ -> compileE e env ++ [Return]
  EAp (EVar "not")    _ -> compileE e env ++ [Return]
  EAp (EAp (EVar op) _) _
    | op `elem` aDomain builtInDyadic
      -> compileE e env ++ [Return]
    | op `elem` ["&&", "||"]
      -> compileE e env ++ [Return]
  EAp (EAp (EAp (EVar "if") e0) e1) e2
    -> compileB e0 env ++ [Cond (compileR e1 env) (compileR e2 env)]
  ECase expr alts
    -> compileE expr env ++ [Casejump (compileD compileAR alts env)]
  _ -> compileE e env ++ [Update n, Pop n, Unwind]
  where n = length env

compileLetrecR :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLetrecR comp defs expr env
  = [Alloc n] ++ compiled defs ++ comp expr env'
  where n = length defs
        env' = compileArgs defs env
        compiled dds = fst $ foldr phi ([], 0) dds
          where phi (_, e) (ds, i) = (compileC e env' ++ [Update i] ++ ds, i+1)

compileLetR :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLetR comp defs expr env
  = compileLet' defs env ++ comp expr env'
  where env' = compileArgs defs env


type GmCompiler = CoreExpr  -> GmEnvironment -> GmCode
type GmEnvironment = Assoc Name Int

--
-- Strict scheme
-- E scheme compiles code that evaluates an expression e to WHNF in the environment,
-- leaving a pointer to the expression on top of the stack.
--
compileE :: GmCompiler
compileE e env = case e of
  ENum i -> [Pushint i]
  ELet recursive defs e
    | recursive -> compileLetrec compileE defs e env
    | otherwise -> compileLet    compileE defs e env
  EAp (EAp (EVar op) _) _
    | op `elem` aDomain builtInDyadic
      -> compileB e env ++ [dyadic]
    where binop = aLookup builtInDyadic op (error "unknown dyadic")
          dyadic | binop `elem` [Add, Sub, Mul, Div] = UpdateInt n
                 | otherwise                         = UpdateBool n
  EAp (EVar "negate") _ -> compileB e env ++ [UpdateInt n]
  EAp (EAp (EVar op) _) _
    | op `elem` ["&&", "||"] -> compileB e env ++ [UpdateBool n]
  EAp (EVar "not") _ -> compileB e env ++ [UpdateBool n]
  EAp (EAp (EAp (EVar "if") e0) e1) e2
    -> compileB e0 env ++ [Cond (compileE e1 env) (compileE e2 env)]
  EAp (EConstr t a) _ -> compileC e env -- in this case, action is as same as compileC's.
  ECase expr alts
    -> compileE expr env ++ [Casejump (compileD compileA alts env)]
  _ -> compileC e env ++ [Eval]
  where n = length env

--
-- D scheme compiles the code for the alternatives in a case expression.
--
compileD :: (Tag -> GmCompiler) -- compiler for alternative bodies
         -> [CoreAlt]           -- the list of alteratives
         -> GmEnvironment       -- the current environment
         -> [(Tag, GmCode)]     -- list of alternative code sequences
compileD comp alts env
  = [ (tag, comp len body (zip names [0..] ++ argOffset len env))
    | (tag, names, body) <- alts
    , let len = length names
    ]

--
-- A scheme compiles the code for an alternative in a case expression.
--
compileA :: Int -> GmCompiler
compileA offset expr env
  = [Split offset] ++ compileE expr env ++ [Slide offset]

compileAR :: Int -> GmCompiler
compileAR offset expr env
  = [Split offset] ++ compileR expr env

builtInDyadic :: Assoc Name Instruction
builtInDyadic
  = [ ("+",  Add)
    , ("-",  Sub)
    , ("*",  Mul)
    , ("/",  Div)
    , ("==", Eq)
    , ("/=", Ne)
    , (">=", Ge)
    , (">",  Gt)
    , ("<=", Le)
    , ("<",  Lt)
    ]

{- |
-- 42
>>> compileC (ENum 42) []
[Pushint 42]

-- let x = 3 in x + x
>>> compileC (ELet False [("x", ENum 3)] (EAp (EAp (EVar "+") (EVar "x")) (EVar "x"))) []
[Pushint 3,Push 0,Push 1,Pushglobal "+",Mkap,Mkap,Slide 1]

-- f
>>> compileC (EVar "f") []
[Pushglobal "f"]

-- f 1
>>> compileC (EAp (EVar "f") (ENum 1)) []
[Pushint 1,Pushglobal "f",Mkap]

-- f 1 2
>>> compileC (EAp (EAp (EVar "f") (ENum 1)) (ENum 2)) []
[Pushint 2,Pushint 1,Pushglobal "f",Mkap,Mkap]

-- f 1 2 a
>>> compileC (EAp (EAp (EAp (EVar "f") (ENum 1)) (ENum 2)) (EVar "a")) []
[Pushglobal "a",Pushint 2,Pushint 1,Pushglobal "f",Mkap,Mkap,Mkap]

-- f (g 1)
>>> compileC (EAp (EVar "f") (EAp (EVar "g") (ENum 1))) []
[Pushint 1,Pushglobal "g",Mkap,Pushglobal "f",Mkap]

-- Pack{1,0}
>>> compileC (EConstr 1 0) []
[Pack 1 0]

-- Pack{1,1} 1
>>> compileC (EAp (EConstr 1 1) (ENum 1)) []
[Pushint 1,Pack 1 1]

-- Pack{1,2} 1 a
>>> compileC (EAp (EAp (EConstr 1 2) (ENum 1)) (EVar "a"))  []
[Pushglobal "a",Pushint 1,Pack 1 2]

-- f (Pack{1,2} 1 a)
>>> compileC (EAp (EVar "f") (EAp (EAp (EConstr 1 2) (ENum 1)) (EVar "a"))) []
[Pushglobal "a",Pushint 1,Pack 1 2,Pushglobal "f",Mkap]

-- f (Pack{1,2} (g 2) a)
>>> compileC (EAp (EVar "f") (EAp (EAp (EConstr 1 2) (EAp (EVar "g") (ENum 2))) (EVar "a"))) []
[Pushglobal "a",Pushint 2,Pushglobal "g",Mkap,Pack 1 2,Pushglobal "f",Mkap]

-- f 0 (Pack{1,2} (g 2) (h 3)) 4
>>> let g2 = EAp (EVar "g") (ENum 2)
>>> let h3 = (EAp (EVar "h") (ENum 3))
>>> compileC (EAp (EAp (EAp (EVar "f") (ENum 0)) (EAp (EAp (EConstr 1 2) g2) h3)) (ENum 4)) []
[Pushint 4,Pushint 3,Pushglobal "h",Mkap,Pushint 2,Pushglobal "g",Mkap,Pack 1 2,Pushint 0,Pushglobal "f",Mkap,Mkap,Mkap]

-- Pack{1,2} 4
>>> compileC (EAp (EConstr 1 2) (ENum 4)) []
[Pushint 4,Pushglobal "Pack{1,2}",Mkap]

-- Pack{1,2} 4 2
>>> compileC (EAp (EAp (EConstr 1 2) (ENum 4)) (ENum 2)) []
[Pushint 2,Pushint 4,Pack 1 2]

-- Pack{1,5} 4
>>> compileC (EAp (EConstr 1 5) (ENum 4)) []
[Pushint 4,Pushglobal "Pack{1,5}",Mkap,Mkap,Mkap,Mkap]

-- Pack{1,5} 4 a
>>> compileC (EAp (EAp (EConstr 1 5) (ENum 4)) (EVar "a")) []
[Pushglobal "a",Pushint 4,Pushglobal "Pack{1,5}",Mkap,Mkap,Mkap]

-- Pack{1,4} 4 a 2 b
>>> let constr4a = EAp (EAp (EConstr 1 4) (ENum 4)) (EVar "a")
>>> compileC (EAp (EAp constr4a (ENum 2)) (EVar "b")) []
[Pushglobal "b",Pushint 2,Pushglobal "a",Pushint 4,Pack 1 4]

-}
--
-- to Code
-- C scheme generates code which constructs the graph of e in the environment,
-- leaving a pointer to it on top of the stack.
--
compileC :: GmCompiler
compileC expr env = case expr of
  (EVar v)
    | v `elem` aDomain env -> [Push n]
    | otherwise            -> [Pushglobal v]
    where n = aLookup env v (error "Can't happen")
  (ENum n)                 -> [Pushint n]
  (EConstr t a)
    | a == 0               -> [Pack t a]
    | otherwise            -> error $ "found invalid Pack arity: " ++ show a
  (EAp _ _)                -> compiled ++ unwrap trailer
    where (compiled, trailer) = compileCS expr env (Right [])
  (ELet recursive defs e)
    | recursive            -> compileLetrec compileC defs e env
    | otherwise            -> compileLet    compileC defs e env
  (ECase _ _)              -> compileE expr env
  _                        -> error $ "support this expr: " ++ show expr
  where
    unwrap = either id id
    -- In the case of normal function application, the trailer is [Mkap, Mkap, ..].
    -- On the other hand, for data constructor, the trailer is [], except for the case of unsatisified.
    -- If data constructor doesn't be saturated, the trailer is [Mkap, Mkap, ..],
    -- which length is the missing arguments length.
    compileCS e ev tl = case e of
      (EConstr t a)
        | saturated -> ([Pack t a], tl')
        | otherwise -> ([Pushglobal (showConstr t a)], tl')
        where tl' = Left (replicate a Mkap) `joint` tl
              saturated = null $ unwrap tl'
      (EAp e1 e2)   -> (compileC e2 ev ++ compiled1, tl')
        where (compiled1, tl') = compileCS e1 (argOffset 1 ev) (Right [Mkap] `joint` tl)
      _             -> (compileC e ev, tl)

    (Left  xs) `joint` (Right ys) = Left  (xs \\ ys)  -- consume Mkap
    (Right xs) `joint` (Right ys) = Right (xs ++ ys)  -- accumulate Mkap
    x          `joint` y          = error $ "unexpected error: " ++ show x ++ " " ++ show y


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

compileLetrec :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLetrec comp defs expr env
  = [Alloc n] ++ compiled defs ++ comp expr env' ++ [Slide n]
  where n = length defs
        env' = compileArgs defs env
        compiled dds = fst $ foldr phi ([], 0) dds
          where phi (_, e) (ds, i) = (compileC e env' ++ [Update i] ++ ds, i+1)

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(v, n+m) | (v, m) <- env]

compileB :: GmCompiler
compileB expr env = case expr of
  (ENum n) -> [Pushbasic n]
  (ELet recursive defs e)
    | recursive -> compileLetrecB compileB defs e env
    | otherwise -> compileLetB    compileB defs e env
  (EAp (EAp (EVar op) e0) e1)
    | op `elem` aDomain builtInDyadic
      -> compileB e1 env ++ compileB e0 env ++ [op']
         where op' = aLookup builtInDyadic op (error "invalid dyadic operator")
  (EAp (EAp (EVar "&&") e0) e1)
    -> compileB e0 env ++ [Cond (compileB e1 env) [Pushbasic 1]] -- 1: False
  (EAp (EAp (EVar "||") e0) e1)
    -> compileB e0 env ++ [Cond [Pushbasic 2] (compileB e1 env)] -- 2: True
  (EAp (EVar "not") e)
    -> compileB e env ++ [Cond [Pushbasic 1] [Pushbasic 2]] -- 1: False, 2: True
  (EAp (EVar "negate") e)
    -> compileB e env ++ [Neg]
  (EAp (EAp (EAp (EVar "if") e0) e1) e2)
    -> compileB e0 env ++ [Cond (compileB e1 env) (compileB e2 env)]
  e -> compileE e env ++ [Get]

compileLetrecB :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLetrecB comp defs expr env
  = [Alloc n] ++ compiled defs ++ comp expr env' ++ [Pop n]
  where n = length defs
        env' = compileArgs defs env
        compiled dds = fst $ foldr phi ([], 0) dds
          where phi (_, e) (ds, i) = (compileC e env' ++ [Update i] ++ ds, i+1)

compileLetB :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLetB comp defs expr env
  = compileLet' defs env ++ comp expr env' ++ [Pop (length defs)]
  where env' = compileArgs defs env

showSimpleResult :: [GmState] -> String
showSimpleResult states = concatMap (iDisplay . outputLast . getOutput) states

showResults :: [GmState] -> String
showResults [] = error "no GmState"
showResults states@(s:ss)
  = unlines (map iDisplay
              ([ iStr "Supercombinator definitions", iNewline
               , iInterleave iNewline (map (showSC s) (getGlobals s))
               , iStr "State transitions"
               ] ++
               iLayn' (map showState states) ++
               [ showStats (last states)
               ])
            )

-- | show dump list
showSC :: GmState -> (Name, Addr) -> IseqRep 
showSC s (name, addr)
  = case hLookup (getHeap s) addr of
      NGlobal arity code
        -> iConcat [ iStr "Code for ", iStr name, iNewline
                   , showInstructions code, iNewline, iNewline
                   ]
      e -> error $ "no NGlobal: " ++ show e

showInstructions :: GmCode -> IseqRep
showInstructions is
  = iConcat [ iStr "  Code: "
            , iIndent (iInterleave iNewline (map showInstruction is))
            ]

showInstruction :: Instruction -> IseqRep
showInstruction i = case i of
  Unwind         -> iStr "Unwind"
  Pushglobal f   -> iStr "Pushglobal " `iAppend` iStr (show f)
  Push n         -> iStr "Push " `iAppend` iNum n
  Pushint n      -> iStr "Pushint " `iAppend` iNum n
  Mkap           -> iStr "Mkap"
  Pop n          -> iStr "Pop " `iAppend` iNum n
  Update n       -> iStr "Update " `iAppend` iNum n
  Slide n        -> iStr "Slide " `iAppend` iNum n
  Alloc n        -> iStr "Alloc " `iAppend` iNum n
  Eval           -> iStr "Eval"
  Add            -> iStr "Add"
  Sub            -> iStr "Sub"
  Mul            -> iStr "Mul"
  Div            -> iStr "Div"
  Neg            -> iStr "Neg"
  Eq             -> iStr "Eq"
  Ne             -> iStr "Ne"
  Lt             -> iStr "Lt"
  Le             -> iStr "Le"
  Gt             -> iStr "Gt"
  Ge             -> iStr "Ge"
  Cond t e       -> iConcat [iStr "Cond "
                            , shortShowInstructions 3 t
                            , shortShowInstructions 3 e
                            ]
  Pack t a       -> iConcat [iStr "Pack " , iNum t, iStr " " , iNum a]
  Casejump bs    -> iConcat [ iStr "Casejump ", showAlts bs]
  Split n        -> iStr "Split " `iAppend` iNum n
  Pushbasic n    -> iStr "Pushbasic " `iAppend` iNum n
  UpdateInt n    -> iStr "UpdateInt" `iAppend` iNum n
  UpdateBool n   -> iStr "UpdateBool" `iAppend` iNum n
  Get            -> iStr "Get"
  Return         -> iStr "Return"
  Print          -> iStr "Print"
  PutChar        -> iStr "PutChar"

showCodes :: [Instruction] -> IseqRep
showCodes []     = iStr "[]"
showCodes (x:xs) = iConcat [iStr "[ ", showInstruction x, iStr ".. ]"]

showAlts :: [(Int, GmCode)] -> IseqRep
showAlts bs = iConcat [ iStr "{"
                      , iInterleave (iStr ",") (map showLabels bs)
                      , iStr "}"
                      ]
  where showLabels (t, c)
          = iConcat [iNum t, iStr ":", shortShowInstructions 2 c]

showState :: GmState -> IseqRep
showState s
  = iConcat [ showOutput s, iNewline
            , showStack s, iNewline
            , showDump s, iNewline
            , showVStack s, iNewline
            , showInstructions (getCode s), iNewline 
            ]

showOutput :: GmState -> IseqRep
showOutput s
  = iConcat [iStr "Output:\"", iStr (outputAll (getOutput s)), iStr "\""]

showDump :: GmState -> IseqRep
showDump s
  = iConcat [ iStr "  Dump: ["
            , iIndent (iInterleave iNewline
                      (map showDumpItem (reverse (S.getStack $ getDump s))))
            , iStr "]"
            ]

showDumpItem :: GmDumpItem -> IseqRep
showDumpItem (code, stack, vstack)
  = iConcat [ iStr "<"
            , shortShowInstructions 3 code, iStr ", "
            , shortShowStack stack, iStr ", "
            , shortShowVStack vstack
            , iStr ">"
            ]

shortShowVStack :: GmVStack -> IseqRep
shortShowVStack s
  = iConcat [ iStr "["
            , iInterleave (iStr ", ") (map iNum $ S.getStack s)
            , iStr "]"
            ]

showVStack :: GmState -> IseqRep
showVStack s
  = iConcat [ iStr "VStack:["
            , iInterleave (iStr ", ") (map iNum (S.getStack (getVStack s)))
            , iStr "]"
            ]

shortShowInstructions :: Int -> GmCode -> IseqRep
shortShowInstructions n code
  = iConcat [ iStr "{"
            , iInterleave (iStr "; ") dotcodes
            , iStr "}"
            ]
    where codes = map showInstruction (take n code)
          dotcodes | length code > n = codes ++ [iStr ".."]
                   | otherwise       = codes

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
showNode s a (NNum n) = iNum n
showNode s a (NGlobal n g) = iConcat [iStr "Global ", iStr v]
  where v = head [n | (n, b) <- getGlobals s, a == b]
showNode s a (NAp a1 a2)
  = iConcat [ iStr "Ap ", iStr (showaddr a1)
            , iStr " ", iStr (showaddr a2)
            ]
showNode s a (NInd a1) = iConcat [ iStr "NInd ", iStr (showaddr a1) ]
showNode s a (NConstr t as)
  = iConcat [ iStr "Constr ", iNum t, iStr " ["
            , iInterleave (iStr ", ") (map (iStr . showaddr) as)
            , iStr "]"
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
>>> let runTest = outputAll . getOutput . last . eval . compile . parse
>>> runTest "main = S K K 3"
"3"

>>> runTest "id = S K K; main = twice twice twice id 3"
"3"

>>> :{
let prog = [ "pair x y f = f x y;"
           , "f p = p K;"
           , "s p = p K1;"
           , "g x y = letrec"
           , "          a = pair x b;"
           , "          b = pair y a"
           , "        in f (s (s (s a)));"
           , "main = g 3 4"
           ]
in runTest (unlines prog)
:}
"4"

>>> runTest "id x = x; main = twice twice twice id 5"
"5"

>>> runTest "main = negate 7"
"-7"

>>> runTest "main = negate (negate 8)"
"8"

>>> runTest "main = ((6 / 2) * 6) + (4 * (10 - 4))"
"42"

>>> runTest "double x = x + x; main = S K K double (double 3)"
"12"

>>> runTest "double x = x + x; main = double (S K K 3)"
"6"

>>> runTest "double x = x + x; sq x = S K K (x * x); main = double (sq 3)"
"18"

>>> runTest "double x = x + x; square x = x * x; main = double (square 3)"
"18"

>>> runTest "main = if True 1 2"
"1"

>>> runTest "main = if False 1 2"
"2"

>>> runTest "main = if (not True) 1 2"
"2"

>>> runTest "main = if (True && False) 1 2"
"2"

>>> runTest "main = if (False || True) 1 2"
"1"

>>> runTest "main = if (xor False True) 1 2"
"1"

>>> runTest "main = if (xor True False) 1 2"
"1"

>>> runTest "main = if (xor True True) 1 2"
"2"

>>> runTest "main = if (xor False False) 1 2"
"2"

>>> runTest "main = if (1 == 1 && 2 /= 3) 1 2"
"1"

>>> runTest "main = if (1 < 2 && 2 <= 1) 1 2"
"2"

>>> runTest "main = if (1 > 2 || 2 >= 1) 1 2"
"1"

>>> runTest "fac n = if (n==0) 1 (n*fac (n-1)); main = fac 5"
"120"

>>> runTest "main = fst (snd (fst (Pair (Pair 1 (Pair 2 3)) 4)))"
"2"

>>> runTest "main = Cons (1+2) (Cons (3+4) Nil)"
"(Pack{2,2} 3 (Pack{2,2} 7 Pack{1,0}))"

>>> :{
let prog = [ "foo x y z = x + y + z;"
           , "bar a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j;"
           , "buz x y z w = x * y * z * w;"
           , "main = foo 1 2 3 + bar 1 2 3 4 5 6 7 8 9 10 + buz 1 2 3 4"
           ]
in runTest (unlines prog)
:}
"85"

>>> :{
let prog = [ "double x = x + x;"
           , "main = Cons (double (double (S K K 3))) Nil"
           ]
in runTest (unlines prog)
:}
"(Pack{2,2} 12 Pack{1,0})"

>>> runTest "main = let id1 = I I I in id1 id1 3"
"3"

>>> :{
let prog = [ "oct g x = let h = twice g"
           , "          in let k = twice h"
           , "             in k (k x);"
           , "main = oct I 4"
           ]
in runTest (unlines prog)
:}
"4"

>>> :{
let prog = [ "cons a b cc cn = cc a b;"
           , "nil      cc cn = cn;"
           , "hd list = list K abort;"
           , "tl list = list K1 abort;"
           , "abort = abort;"
           , "infinite x = letrec xs = cons x xs"
           , "             in xs;"
           , "main = hd (tl (tl (infinite 4)))"
           ]
in runTest (unlines prog)
:}
"4"

>>> runTest "main = 4*5+(2-5)"
"17"

>>> runTest "inc x = x+1; main = twice twice inc 4"
"8"

>>> :{
let prog = [ "cons a b cc cn = cc a b;"
           , "nil      cc cn = cn;"
           , "length xs = xs length1 0;"
           , "length1 x xs = 1 + length xs;"
           , "main = length (cons 42 (cons 42 (cons 42 nil)))"
           ]
in runTest (unlines prog)
:}
"3"

>>> :{
let prog = [ "gcd2 a b = if (a == b)"
           , "              a"
           , "              if (a < b) (gcd2 b a) (gcd2 b (a - b));"
           , "main = gcd2 485 1261"
           ]
in runTest (unlines prog)
:}
"97"

let prog = [ "fib n = if (n < 2)"
           , "           1"
           , "           (fib (n - 1) + fib (n - 2)) ;"
           , "main = fib 10"
           ]
in runTest (unlines prog)
:}
"89"

>>> runTest "f = negate; main = f 5"
"-5"

>>> :{
let prog = [ "myIf c t f = case c of"
           , "  <1> -> f;"
           , "  <2> -> t;"
           , "fib n = myIf (n < 2)"
           , "             1"
           , "             (fib (n - 1) + fib (n - 2));"
           , "main = fib 10"
           ]
in runTest (unlines prog)
:}
"89"

>>> :{
let prog = [ "T = Pack{2,0};"
           , "F = Pack{1,0};"
           , "myIf c t f = case c of"
           , "  <1> -> f;"
           , "  <2> -> t;"
           , "isThree x = myIf True x (x + 1);"
           , "main = isThree 3"
           ]
in runTest (unlines prog)
:}
"3"

>>> runTest "main = Pack{2,2} 1 Pack{1,0}"
"(Pack{2,2} 1 Pack{1,0})"

>>> runTest "main = (Pack{2,2} 1 (Pack{2,2} 2 (Pack{2,2} 3 Pack{1,0})))"
"(Pack{2,2} 1 (Pack{2,2} 2 (Pack{2,2} 3 Pack{1,0})))"

>>> runTest "nil = Pack{1,0}; cons x xs = Pack{2,2} x xs; main = cons 1 nil"
"(Pack{2,2} 1 Pack{1,0})"

>>> runTest "nil = Pack{1,0}; cons x xs = Pack{2,2} x xs; main = cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil))))"
"(Pack{2,2} 1 (Pack{2,2} 2 (Pack{2,2} 3 (Pack{2,2} 4 (Pack{2,2} 5 Pack{1,0})))))"

>>> :{
let prog = [ "iota x y s = if (x > y) Nil (Cons x (iota (x+s) y s));"
           , "main = iota 5 100 3"
           ]
in runTest (unlines prog)
:}
"(Pack{2,2} 5 (Pack{2,2} 8 (Pack{2,2} 11 (Pack{2,2} 14 (Pack{2,2} 17 (Pack{2,2} 20 (Pack{2,2} 23 (Pack{2,2} 26 (Pack{2,2} 29 (Pack{2,2} 32 (Pack{2,2} 35 (Pack{2,2} 38 (Pack{2,2} 41 (Pack{2,2} 44 (Pack{2,2} 47 (Pack{2,2} 50 (Pack{2,2} 53 (Pack{2,2} 56 (Pack{2,2} 59 (Pack{2,2} 62 (Pack{2,2} 65 (Pack{2,2} 68 (Pack{2,2} 71 (Pack{2,2} 74 (Pack{2,2} 77 (Pack{2,2} 80 (Pack{2,2} 83 (Pack{2,2} 86 (Pack{2,2} 89 (Pack{2,2} 92 (Pack{2,2} 95 (Pack{2,2} 98 Pack{1,0}))))))))))))))))))))))))))))))))"

>>> :{
let prog = [ "ones = Cons 1 ones;"
           , "head xs = case xs of"
           , "  <1> -> Pack{1,0};"
           , "  <2> a b -> Pack{2,1} a;"
           , "main = head ones"
           ]
in runTest (unlines prog)
:}
"(Pack{2,1} 1)"

>>> :{
let prog = [ "plus x y = x + y;"
           , "sum xs = foldr plus 0 xs;"
           , "iota x y = if (x > y) Nil (Cons x (iota (x+1) y));"
           , "double x = 2 * x;"
           , "main = sum (map double (iota 1 10))"
           ]
in runTest (unlines prog)
:}
"110"

>>> :{
let prog = [ "stream n = Cons n (stream (n + 1));"
           , "take n xs = case xs of"
           , "  <1> -> Nil;"
           , "  <2> y ys -> if (n == 0) Nil (Cons y (take (n-1) ys));"
           , "main = take 10 (stream 1)"
           ]
in runTest (unlines prog)
:}
"(Pack{2,2} 1 (Pack{2,2} 2 (Pack{2,2} 3 (Pack{2,2} 4 (Pack{2,2} 5 (Pack{2,2} 6 (Pack{2,2} 7 (Pack{2,2} 8 (Pack{2,2} 9 (Pack{2,2} 10 Pack{1,0}))))))))))"

>>> :{
let prog = [ "mod x y = let d = x / y in x - (d * y);"
           , "gcd a b = let d = mod a b"
           , "          in if (d == 0) b (gcd b d);"
           , "lcm a b = let d = gcd a b"
           , "          in let xa = a / d;"
           , "                 xb = b / d"
           , "             in d * xa * xb;"
           , "main = let a = 2*3*5  *11;"
           , "           b =   3*5*7"
           , "       in Pair (gcd a b) (lcm a b)"
           ]
in runTest (unlines prog)
:}
"(Pack{0,2} 15 2310)"

>>> :{
let prog = [ "tak x y z = if (x <= y)"
           , "               y"
           , "               (tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y));"
           , "main = tak 12 6 0"
           ]
in runTest (unlines prog)
:}
"12"

>>> runTest "main = seq (putStr (showBool (not True))) (putChar 10)"
"False\n10"

>>> :{
let prog = [ "ch x xs = Cons x xs;"
           , "n = Nil;"
           , "H = 72;"
           , "e = 101;"
           , "l = 108;"
           , "o = 111;"
           , "comma = 44;"
           , "W = 87;"
           , "r = 114;"
           , "d = 100;"
           , "ban = 33;"
           , "hello = ch H (ch e (ch l (ch l (ch o (ch comma (ch W (ch o (ch r (ch l (ch d (ch ban n)))))))))));"
           , "main = putStrLn hello"
           ]
in runTest (unlines prog)
:}
"Hello,World!\n10"

>>> :{
let prog = [ "psi n = Just (Pair n (n+1));"
           , "take n xs = case xs of"
           , "  <1> -> Nil;"
           , "  <2> y ys -> if (n == 0) Nil (Cons y (take (n-1) ys));"
           , "main = take 10 (unfoldr psi 1)"
           ]
in runTest (unlines prog)
:}
"(Pack{2,2} 1 (Pack{2,2} 2 (Pack{2,2} 3 (Pack{2,2} 4 (Pack{2,2} 5 (Pack{2,2} 6 (Pack{2,2} 7 (Pack{2,2} 8 (Pack{2,2} 9 (Pack{2,2} 10 Pack{1,0}))))))))))"

>>> :{
let prog = [ "showPair p = case p of"
           , "  <0> f s -> let nr     = putChar 10;"
           , "                 lparen = putChar 40;"
           , "                 rparen = putChar 41;"
           , "                 comma  = putChar 44"
           , "             in let body = Cons (putNumber f) (Cons comma (Cons (putNumber s) Nil))"
           , "                in foldr seq nr (bracket lparen body rparen);"
           , "seq x y = y + 0 * x;"
           , "main = showPair (Pair 42 28)"
           ]
in runTest (unlines prog)
:}
"(42,28)\n10"

>>> :{
let prog = [ "nr = putChar 10;"
           , "lparen = putChar 91;"
           , "rparen = putChar 93;"
           , "iota x y = if (x > y) Nil (Cons x (iota (x+1) y));"
           , "body = intersperse (putChar 44) (map putNumber (iota 1 10));"
           , "main = foldr seq nr (bracket lparen body rparen)"
           ]
in runTest (unlines prog)
:}
"[1,2,3,4,5,6,7,8,9,10]\n10"

>>> :{
let prog = [ "from n = Cons n (from (n+1));"
           , "take n xxs = case xxs of"
           , "  <1> -> Nil;"
           , "  <2> x xs -> if (n == 0) Nil (Cons x (take (n-1) xs));"
           , "main = putList putNumber (take 10 (from 1))"
           ]
in runTest (unlines prog)
:}
"[1,2,3,4,5,6,7,8,9,10]93"

>>> :{
let prog = [ "iota s e = if (s>e) Nil (Cons s (iota (s+1) e));"
           , "main = putList (putPair putChar putNumber) (map dup (iota 32 127))"
           ]
in runTest (unlines prog)
:}
"[( ,32),(!,33),(\",34),(#,35),($,36),(%,37),(&,38),(',39),((,40),(),41),(*,42),(+,43),(,,44),(-,45),(.,46),(/,47),(0,48),(1,49),(2,50),(3,51),(4,52),(5,53),(6,54),(7,55),(8,56),(9,57),(:,58),(;,59),(<,60),(=,61),(>,62),(?,63),(@,64),(A,65),(B,66),(C,67),(D,68),(E,69),(F,70),(G,71),(H,72),(I,73),(J,74),(K,75),(L,76),(M,77),(N,78),(O,79),(P,80),(Q,81),(R,82),(S,83),(T,84),(U,85),(V,86),(W,87),(X,88),(Y,89),(Z,90),([,91),(\\,92),(],93),(^,94),(_,95),(`,96),(a,97),(b,98),(c,99),(d,100),(e,101),(f,102),(g,103),(h,104),(i,105),(j,106),(k,107),(l,108),(m,109),(n,110),(o,111),(p,112),(q,113),(r,114),(s,115),(t,116),(u,117),(v,118),(w,119),(x,120),(y,121),(z,122),({,123),(|,124),(},125),(~,126),(\DEL,127)]93"

>>> :{
let prog = [ "num2chr n = unfoldr psi n;"
           , "psi n = if (n == 0) Nothing (Just (swap (divMod n 10)));"
           , "revchr xs = foldr phi I xs Nil;"
           , "phi b g x = g (Cons (48 + b) x);"
           , "showNum n = revchr (num2chr n);"
           , "main = putStrLn (showNum 257)"
           ]
in runTest (unlines prog)
:}
"257\n10"

>>> :{
let prog = [ "x1 = Pair Nothing (Just 42);"
           , "x2 = let xs = Cons 65 (Cons 66 (Cons 67 Nil)) in Pair (Left xs) (Right xs);"
           , "t1 = let p = putMaybe putNumber in putPair p p x1;"
           , "t2 = let p = putEither (putList putNumber) (putList putChar) in putPair p p x2;"
           , "main = seq t1 t2"
           ]
in runTest (unlines prog)
:}
"(Nothing,Just 42)(Left [65,66,67],Right [A,B,C])0"

>>> runTest "apply f x = f x; test x = apply (Pack{2,2} 42) x; main = test 4"
"(Pack{2,2} 42 4)"

>>> :{
let prog = [ "prefix p xs = map (f p) xs;"
           , "f p x = Pack{2,2} p x;"
           , "main = prefix 42 (Cons 1 Nil)"
           ]
in runTest (unlines prog)
:}
"(Pack{2,2} (Pack{2,2} 42 1) Pack{1,0})"

>>> :{
let prog = [ "prefix p xs = map (Pack{2,2} p) xs;"
           , "main = prefix 42 (Cons 1 (Cons 2 (Cons 3 Nil)))"
           ]
in runTest (unlines prog)
:}
"(Pack{2,2} (Pack{2,2} 42 1) (Pack{2,2} (Pack{2,2} 42 2) (Pack{2,2} (Pack{2,2} 42 3) Pack{1,0})))"

>>> runTest "f x = Pack{2,2} (case x of <1> -> 1; <2> -> 2) Pack{1,0}; main = f Pack{2,0}"
"(Pack{2,2} 2 Pack{1,0})"

>>> :{
let prog = [ "pair x y f = f x y;"
           , "f p = p K;"
           , "s p = p K1;"
           , "g x y = letrec"
           , "          a = pair x b;"
           , "          b = pair y a"
           , "        in f (s (s (s a)));"
           , "main = g 3 4"
           ]
in runTest (unlines prog)
:}
"4"

>>> :{
let prog = [ "pair x y f = f x y;"
           , "f p = p K;"
           , "s p = p K1;"
           , "g x y z = letrec"
           , "            a = pair x b;"
           , "            b = pair y c;"
           , "            c = pair z a"
           , "          in f (s (s (s a)));"
           , "main = g 3 4 5"
           ]
in runTest (unlines prog)
:}
"3"

>>> :{
let prog = [ "undefined = undefined;"
           , "fact0 n = if (n == 0) 1 (n * undefined (n-1));"
           , "fact1 n = if (n == 0) 1 (n * fact0 (n-1));"
           , "fact2 n = if (n == 0) 1 (n * fact1 (n-1));"
           , "fact3 n = if (n == 0) 1 (n * fact2 (n-1));"
           , "fact4 n = if (n == 0) 1 (n * fact3 (n-1));"
           , "fact5 n = if (n == 0) 1 (n * fact4 (n-1));"
           , "fact6 n = if (n == 0) 1 (n * fact5 (n-1));"
           , "fact7 n = if (n == 0) 1 (n * fact6 (n-1));"
           , "fact8 n = if (n == 0) 1 (n * fact7 (n-1));"
           , "fact9 n = if (n == 0) 1 (n * fact8 (n-1));"
           , "fact  n = if (n == 0) 1 (n * fact9 (n-1));"
           , "main = fact 10"
           ]
in runTest (unlines prog)
:}
"3628800"

>>> :{
let prog = [ "F f n = if (n == 0) 1 (n * f (n-1));"
           , "fac = F fac;"
           , "main = fac 10"
           ]
in runTest (unlines prog)
:}
"3628800"

>>> :{
let prog = [ "Y f = letrec x = f x in x;"
           , "facF f n = if (n == 0) 1 (n * f (n-1));"
           , "fac = Y facF;"
           , "main = fac 10"
           ]
in runTest (unlines prog)
:}
"3628800"

>>> :{
let prog = [ "B f g x = f (g x);"
           , "G a b c d = let e = c d in a (b e) e;"
           , "fmap f x = case x of"
           , "  <0>   -> Nothing;"
           , "  <1> y -> Just (f y);"
           , "Zero = Pack{0,0};"
           , "Succ n = Pack{1,1} n;"
           , "foldn c f n = case n of"
           , "  <0>   -> c;"
           , "  <1> m -> f (foldn c f m);"
           , "unfoldn psi x = case psi x of"
           , "  <0> -> Zero;"
           , "  <1> y -> Succ (unfoldn psi y);"
           , "out n = case n of"
           , "  <0>   -> Nothing;"
           , "  <1> m -> Just m;"
           , "unit x = Pack{1,1} x;"
           , "cons x y = Pack{2,2} x y;"
           , "unwrap nel = case nel of"
           , "  <1> x    -> Left x;"
           , "  <2> x xs -> Right (Pair x xs);"
           , "fold c f nel = case nel of"
           , "  <1> x   -> c x;"
           , "  <2> x y -> f x (fold c f y);"
           , "unfold psi x = case psi x of"
           , "  <1> a -> unit a;"
           , "  <2> p -> case p of"
           , "    <2> a b -> cons a (unfold psi b);"
           , "extract x = case x of"
           , "  <1> x   -> x;"
           , "  <2> x y -> x;"
           , "sub x = case unwrap x of"
           , "  <1> x -> Nothing;"
           , "  <2> p -> case p of"
           , "    <0> a b -> Just b;"
           , "histo phi = letrec"
           , "              u = G (S (B maybe unit) cons) phi (B (fmap u) out)"
           , "            in B extract u;"
           , "dyna f g = B (histo f) (unfoldn g);"
           , "phi x = case x of"
           , "  <0>   -> 0;"
           , "  <1> x -> extract x + maybe 1 extract (sub x);"
           , "psi n = if (n == 0) Nothing (Just (n-1));"
           , "fib n = dyna phi psi n;"
           , "main = fib 20"
           ]
in runTest (unlines prog)
:}
"6765"

>>> :{
let prog = [ "Zero = Pack{0,0};"
           , "Succ n = Pack{1,1} n;"
           , "foldn c f n = case n of"
           , "  <0>   -> c;"
           , "  <1> m -> f (foldn c f m);"
           , "add x y = foldn x Succ y;"
           , "next p = case p of"
           , "  <0> x y -> Pair y (add x y);"
           , "fib n = fst (foldn (Pair Zero (Succ Zero)) next n);"
           , "main = fib (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))"
           ]
in runTest (unlines prog)
:}
"(Pack{1,1} (Pack{1,1} (Pack{1,1} (Pack{1,1} (Pack{1,1} (Pack{1,1} (Pack{1,1} (Pack{1,1} (Pack{1,1} (Pack{1,1} (Pack{1,1} (Pack{1,1} (Pack{1,1} Pack{0,0})))))))))))))"

>>> :{
let prog = [ "Zero = Pack{0,0};"
           , "Succ n = Pack{1,1} n;"
           , "foldn c f n = case n of"
           , "  <0>   -> c;"
           , "  <1> m -> f (foldn c f m);"
           , "add x y = foldn y Succ x;"
           , "toNat i = if (i == 0) Zero (Succ (toNat (i-1)));"
           , "fromNat n = case n of"
           , "  <0> -> 0;"
           , "  <1> n -> 1 + fromNat n;"
           , "next p = case p of"
           , "  <0> x y -> Pair y (add x y);"
           , "fib n = fst (foldn (Pair Zero (Succ Zero)) next n);"
           , "main = fromNat (fib (toNat 10))"
           ]
in runTest (unlines prog)
:}
"55"
-}
