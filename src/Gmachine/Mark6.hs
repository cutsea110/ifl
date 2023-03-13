{-# LANGUAGE NPlusKPatterns  #-}
module Gmachine.Mark6
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

runProg :: String -> String
runProg = showResults . eval . compile . parse

data GmState = GmState { output  :: GmOutput
                       , code    :: GmCode
                       , stack   :: GmStack
                       , dump    :: GmDump
                       , heap    :: GmHeap
                       , globals :: GmGlobals
                       , stats   :: GmStats
                       }

type GmOutput = [String]
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
  | Push Int -- push offset
  | Pop Int  -- pop offset
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
  | Print
  | PutChar
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
dispatch (Pack t n)     = pack t n
dispatch (Casejump bs)  = casejump bs
dispatch (Split n)      = split n
dispatch Print          = gmprint
dispatch PutChar        = putchar

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
        (h', a) = hAlloc (getHeap state) (NConstr b' [])
        b' | b         = 2 -- tag of True
           | otherwise = 1 -- tag of False

comparison :: (Int -> Int -> Bool) -> GmState -> GmState
comparison = primitive2 boxBoolean unboxInteger

cond :: GmCode -> GmCode -> GmState -> GmState
cond i1 i2 state = case hLookup heap a of
  NNum 1 -> putCode (i1 ++ i) $ putStack stack' state
  NNum 0 -> putCode (i2 ++ i) $ putStack stack' state
  e      -> error $ "unexpected non-integer in heap: " ++ show e
  where heap = getHeap state
        stack = getStack state
        i = getCode state
        (a, stack') = S.pop stack

pack :: Tag -> Arity -> GmState -> GmState
pack t n state
  = state { stack = S.push a s'
          , heap  = h'
          }
  where (as, s') = S.nPop n (getStack state)
        d = NConstr t as
        h = getHeap state
        (h', a) = hAlloc h d

casejump :: [(Tag, GmCode)] -> GmState -> GmState
casejump bs state = state { code = i' ++ i
                          }
  where (a, _) = S.pop (getStack state)
        i = getCode state
        h = getHeap state
        d = hLookup h a
        i' = case d of
          NConstr t _
            -> aLookup bs t (error "unknown tag")
          _ -> error "not data structure"

split :: Int -> GmState -> GmState
split n state = state { stack = s''
                      }
  where (a, s') = S.pop (getStack state)
        d = hLookup (getHeap state) a
        s'' = case d of
          NConstr t as
            | length as == n -> foldr S.push s' as
            | otherwise -> error "non-saturated"
          _ -> error "not data structure"

gmprint :: GmState -> GmState
gmprint state = case hLookup h a of
          NNum n -> state { output = o ++ [show n]
                          , stack = s
                          }
          NConstr t as -> state { output = o ++ [lparen ++ showConstr t (length as)]
                                , code = printcode as ++ rparen ++ i
                                , stack = foldr S.push s as
                                }
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
  NNum n -> state { output = o ++ [[chr n]]
                  , stack = s
                  }
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
  Nothing
    -> state { stack = S.push a stack
             }

  Just (Pack tag arity)
    | f `elem` aDomain globals
      -> state { stack = S.push a stack
               }
    | otherwise
      -> state { stack = S.push a stack
               , heap = h'
               , globals = aInsert globals (showConstr tag arity) a
               }
    where a = aLookup globals f a'
          (h', a') = hAlloc heap gNode
          gNode = NGlobal arity [Pack tag arity, Update 0, Unwind]
  Just _ -> error "pushglobal: not Pack"
  where stack = getStack state
        heap = getHeap state
        globals = getGlobals state
        a = aLookup globals f (error $ "Undeclared global " ++ f)

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
        newState (NNum n)
          | S.isEmpty dump = putCode [] state
          | otherwise      = state { code = i'
                                   , stack = S.push a s'
                                   , dump = d
                                   }
          where ((i', s'), d) = S.pop dump
        newState (NAp a1 a2) = putCode [Unwind] (putStack (S.push a1 s) state)
        newState (NGlobal n c)
          | k < n     = putCode i' (putStack (S.push ak s') (putDump d state))
          | otherwise = putCode c (putStack (rearrange n heap s) state)
          where ((i', s'), d) = S.pop dump
                k             = S.getDepth s1
                (ak, _)       = S.pop (S.discard k s)
        newState (NInd a1) = putCode [Unwind] (putStack (S.push a1 s1) state)
        newState (NConstr _ _)
          | S.isEmpty dump = putCode [] state
          | otherwise      = state { code = i'
                                   , stack = S.push a s'
                                   , dump = d
                                   }
          where ((i', s'), d) = S.pop dump


compile :: CoreProgram -> GmState
compile program = GmState { output = []
                          , code = initialCode
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
    compiled = map compileSc (preludeDefs ++ extraPreludeDefs ++ program) ++ compiledPrimitives
    -- compiled = map compileSc program

extraPreludeCode :: String
extraPreludeCode
  = unlines [ "flip f x y = f y x;"
            , "plus x y = x + y;"
            , "divMod x y = let d = x / y in Pair d (x-d*y);"

            , "False = Pack{1,0};"
            , "True  = Pack{2,0};"
            , "showBool b = case b of"
            , "  <1> -> Cons 70 (Cons 97 (Cons 108 (Cons 115 (Cons 101 Nil))));"
            , "  <2> -> Cons 84 (Cons 114 (Cons 117 (Cons 101 Nil)));"
            , "if c t f = case c of"
            , "               <1> -> f;"
            , "               <2> -> t;"
            , "not x = case x of"
            , "  <1> -> True;"
            , "  <2> -> False;"
            , "and x y = case x of"
            , "  <1> -> False;"
            , "  <2> -> y;"
            , "or x y = case x of"
            , "  <2> -> True;"
            , "  <1> -> y;"
            , "xor x y = case x of"
            , "  <1> -> y;"
            , "  <2> -> not y;"

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

extraPreludeDefs :: CoreProgram
extraPreludeDefs = parse extraPreludeCode

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

    , ("putNumber", 1, [Push 0, Eval, Print, Unwind])
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
-}
compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC
compileSc (name, env, body) = (name, length env, compileR body (zip env [0..]))

-- maybe Reduction's R
compileR :: GmCompiler
compileR e env = compileE e env ++ [Update n, Pop n, Unwind]
  where n = length env

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
  EAp (EAp (EVar op) e0) e1
    | op `elem` aDomain builtInDyadic
      -> compileE e1 env ++
         compileE e0 (argOffset 1 env) ++
         [dyadic]
    where dyadic = aLookup builtInDyadic op (error "unknown dyadic")
  EAp (EVar "negate") e0 -> compileE e0 env ++ [Neg]
  EAp (EConstr t a) _ -> compileC e env -- in this case, action is as same as compileC's.
  ECase expr alts
    -> compileE expr env ++ [Casejump (compileAlts compileE' alts env)]
  _ -> compileC e env ++ [Eval]

--
-- D scheme compiles the code for the alternatives in a case expression.
--
compileAlts :: (Tag -> GmCompiler) -- compiler for alternative bodies
            -> [CoreAlt]           -- the list of alteratives
            -> GmEnvironment       -- the current environment
            -> [(Tag, GmCode)]     -- list of alternative code sequences
compileAlts comp alts env
  = [ (tag, comp len body (zip names [0..] ++ argOffset len env))
    | (tag, names, body) <- alts
    , let len = length names
    ]

--
-- A scheme compiles the code for an alternative in a case expression.
--
compileE' :: Int -> GmCompiler
compileE' offset expr env
  = [Split offset] ++ compileE expr env ++ [Slide offset]

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
    -- In the case of normal function application, the trailer is [Mkap, Mkap, ...].
    -- On the other hand, for data constructor, the trailer is [], except for the case of unsatisified.
    -- If data constructor doesn't be saturated, the trailer is [Mkap, Mkap, ...],
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

    (Left  xs) `joint` (Right ys) = Left (xs \\ ys)   -- consume Mkap
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

{- |
>>> compileSc . head . parse $ "Y f = letrec x = f x in x"
("Y",1,[Alloc 1,Push 0,Push 2,Mkap,Update 0,Push 0,Eval,Slide 1,Update 1,Pop 1,Unwind])

>>> compileSc . head . parse $ "f x y = letrec a = Pack{0,2} x b; b = Pack{0,2} y a in fst (snd (snd a))"
("f",2,[Alloc 2,Push 0,Push 3,Pack 0 2,Update 1,Push 1,Push 4,Pack 0 2,Update 0,Push 1,Pushglobal "snd",Mkap,Pushglobal "snd",Mkap,Pushglobal "fst",Mkap,Eval,Slide 2,Update 2,Pop 2,Unwind])
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
showInstruction (Cond t e)     = iConcat [iStr "Cond "
                                         , shortShowInstructions 3 t
                                         , shortShowInstructions 3 e
                                         ]
showInstruction (Pack t a)     = iConcat [iStr "Pack " , iNum t, iStr " " , iNum a]
showInstruction (Casejump bs)  = iConcat [ iStr "Casejump ", showAlts bs]
showInstruction (Split n)      = iStr "Split " `iAppend` iNum n
showInstruction Print          = iStr "Print"
showInstruction PutChar        = iStr "PutChar"

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
            , showInstructions (getCode s), iNewline 
            ]

showOutput :: GmState -> IseqRep
showOutput s
  = iConcat [iStr "Output:\"", iStr (concat (getOutput s)), iStr "\""]

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
>>> let runTest = concat . getOutput . last . eval . compile . parse
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

>>> runTest "main = if (and True False) 1 2"
"2"

>>> runTest "main = if (or False True) 1 2"
"1"

>>> runTest "main = if (xor False True) 1 2"
"1"

>>> runTest "main = if (xor True False) 1 2"
"1"

>>> runTest "main = if (xor True True) 1 2"
"2"

>>> runTest "main = if (xor False False) 1 2"
"2"

>>> runTest "main = if (and (1 == 1) (2 /= 3)) 1 2"
"1"

>>> runTest "main = if (and (1 < 2) (2 <= 1)) 1 2"
"2"

>>> runTest "main = if (or (1 > 2) (2 >= 1)) 1 2"
"1"

>>> runTest "fac n = if (n==0) 1 (n*fac (n-1)); main = fac 3"
"6"

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

>>> runTest "double x = x + x; main = Cons (double (double (S K K 3))) Nil"
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
-}
