module Language where

import Utils

data Expr a
  = EVar Name
  | ENum Int
  | EConstr Int Int -- ^ tag and arity
  | EAp (Expr a) (Expr a)
  | ELet IsRec [(a, Expr a)] (Expr a)
  | ECase (Expr a) [Alter a]
  | ELam [a] (Expr a)
  deriving (Show)

type CoreExpr = Expr Name
type Name = String
type IsRec = Bool
recursive :: IsRec
recursive = True
nonRecursive :: IsRec
nonRecursive = False

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [ name | (name, rhs) <- defns ]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [ rhs | (name, rhs) <- defns ]

type Alter a = (Int, [a], Expr a)
type CoreAlter = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

-- pretty printer
pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)

class Iseq iseq where
  iNil :: iseq
  iStr :: String -> iseq
  iAppend :: iseq -> iseq -> iseq
  iNewline :: iseq
  iIndent :: iseq -> iseq
  iDisplay :: iseq -> String

infixr 5 `iAppend`

data Iseqrep = INil
             | IStr String
             | IAppend Iseqrep Iseqrep
             | IIndent Iseqrep
             | INewline
             deriving Show

type Precedence = Int
data Associativity = Infix | InfixL | InfixR deriving (Show, Enum, Eq)
data PrecAssoc = PrecAssoc { weakp :: Precedence -> Associativity -> Bool
                           , prec  :: Precedence
                           , assoc :: Associativity
                           }

-- 右辺値の最上位の式か下位の式か
data Level = Root | Sub deriving (Eq, Show)

instance Iseq Iseqrep where
  iNil              = INil
  iStr []           = INil
  iStr s            = case break (=='\n') s of
    (x, [])  -> IStr s
    (x, _:y) -> IStr x `iAppend` INewline `iAppend` iStr y
  iAppend INil seq  = seq
  iAppend seq  INil = seq
  iAppend seq1 seq2 = IAppend seq1 seq2
  iNewline          = INewline
  iIndent seq       = IIndent seq
  iDisplay seq      = flatten 0 [(seq, 0)]

flatten :: Int -> [(Iseqrep, Int)] -> String
flatten col [] = ""
flatten col ((INil, indent) : seqs)
  = flatten col seqs
flatten col ((IStr s, indent) : seqs)
  = s ++ flatten (col + length s) seqs
flatten col ((IAppend seq1 seq2, indent) : seqs)
  = flatten col ((seq1, indent) : (seq2, indent) : seqs)
flatten col ((INewline, indent) : seqs)
  = '\n' : (space indent ++ flatten indent seqs)
flatten col ((IIndent seq, indent) : seqs)
  = flatten col ((seq, col) : seqs)

space :: Int -> String
space n = replicate n ' '

{- |
>>> double = EVar "double"
>>> _42 = ENum 42

>>> printScDefn ("double", ["x"], x `add` x)
double x = x + x

>>> printScDefn ("main", [], ap double _42)
main = double 42

>>> printScDefn ("f", ["x"], ELet nonRecursive [("y", x `add` _1), ("z", y `add` _2)] z)
f x = let
        y = x + 1;
        z = y + 2
      in z

>>> let (bool, i, n) = (EVar "bool", EVar "i", EVar "n")
>>> let func = ELam ["f", "i"] (bool `ap` (i `mul` (f `ap` (i `sub` _1))) `ap` _1 `ap` (i `eq` _1))
>>> printScDefn ("fact", ["n"], ELet recursive [("y", ELam ["x"] (x `ap` (y `ap` x)))] (y `ap` func `ap` n))
fact n = letrec
           y = \ x -> x (y x)
         in y (\ f i -> bool (i * f (i - 1)) 1 (i == 1)) n

>>> let (foldr, xs, y, ys) = (EVar "foldr", EVar "xs", EVar "y", EVar "ys")
>>> let caseExpr = ECase xs [(1, [], c), (2, ["y", "ys"], f `ap` y `ap` (foldr `ap` c `ap` f `ap` ys))]
>>> printScDefn ("foldr", ["c", "f", "xs"], caseExpr)
foldr c f xs = case xs of
                 <1> -> c ;
                 <2> y ys -> f y (foldr c f ys)

>>> let (unfoldr, psi, xs, y, ys) = (EVar "unfoldr", EVar "psi", EVar "xs", EVar "y", EVar "ys")
>>> let caseExpr = ECase (psi `ap` xs) [(1, [], EConstr 1 0), (2, ["y", "ys"], EConstr 2 2 `ap` y `ap` (unfoldr `ap` psi `ap` ys))]
>>> printScDefn ("unfoldr", ["psi", "xs"], caseExpr)
unfoldr psi xs = case psi xs of
                   <1> -> Pack{1,0} ;
                   <2> y ys -> Pack{2,2} y (unfoldr psi ys)
-}
pprProgram :: CoreProgram -> Iseqrep
pprProgram scdefns = iInterleave iNL $ map pprScDefn scdefns

pprScDefn :: CoreScDefn -> Iseqrep
pprScDefn (name, args, expr)
  = iConcat [ iStr name, sep, pprArgs args
            , iStr " = "
            , iIndent (pprExpr Root defaultPrecAssoc expr)
            ]
    where sep = if null args then iNil else iSpace

iParen :: Iseq iseq => iseq -> iseq
iParen seq = iStr "(" `iAppend` seq `iAppend` iStr ")"

iSpace :: Iseq iseq => iseq
iSpace = iStr " "

pprArgs :: [String] -> Iseqrep
pprArgs args = iInterleave iSpace $ map iStr args

precAssoc :: String -> PrecAssoc
precAssoc "*"  = PrecAssoc { weakp = \p a -> p >  5 || p == 5 && a /= InfixR, prec = 5, assoc = InfixR }
precAssoc "/"  = PrecAssoc { weakp = \p a -> p >= 5, prec = 5, assoc = Infix  }
precAssoc "+"  = PrecAssoc { weakp = \p a -> p >  4 || p == 4 && a /= InfixR, prec = 4, assoc = InfixR }
precAssoc "-"  = PrecAssoc { weakp = \p a -> p >= 4, prec = 4, assoc = Infix  }
precAssoc "==" = PrecAssoc { weakp = \p a -> p >  3, prec = 3, assoc = Infix  }
precAssoc "/=" = PrecAssoc { weakp = \p a -> p >  3, prec = 3, assoc = Infix  }
precAssoc ">"  = PrecAssoc { weakp = \p a -> p >  3, prec = 3, assoc = Infix  }
precAssoc ">=" = PrecAssoc { weakp = \p a -> p >  3, prec = 3, assoc = Infix  }
precAssoc "<"  = PrecAssoc { weakp = \p a -> p >  3, prec = 3, assoc = Infix  }
precAssoc "<=" = PrecAssoc { weakp = \p a -> p >  3, prec = 3, assoc = Infix  }
precAssoc "&&" = PrecAssoc { weakp = \p a -> p >  2, prec = 2, assoc = InfixR }
precAssoc "||" = PrecAssoc { weakp = \p a -> p >  1, prec = 1, assoc = InfixR }
precAssoc _    = error "Unknown infix operator"

defaultPrecAssoc :: PrecAssoc
defaultPrecAssoc = PrecAssoc { weakp = \p a -> False, prec = 0, assoc = Infix }  -- FIXME!
functionPrecAssoc :: PrecAssoc
functionPrecAssoc = PrecAssoc { weakp = \p a -> p > 6, prec = 6, assoc = InfixL }
functionArgPrecAssoc :: PrecAssoc
functionArgPrecAssoc = PrecAssoc { weakp = \p a -> p >= 6, prec = 6, assoc = InfixL }

infixOperator :: String -> Bool
infixOperator op
  = op `elem` [ "*", "/"
              , "+", "-"
              , "==", "/="
              , ">", ">=", "<", "<="
              , "&&", "||"
              ]

{- $setup
>>> [a, b, c, x, y, z, w, f, g, h, p, q, r, s] = map (EVar . (:[])) "abcxyzwfghpqrs"
>>> [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9] = map ENum [0 .. 9]
>>> inc = EAp (EVar "+") _1
>>> dec = EAp (EVar "-") _1
>>> add x y = EAp (EAp (EVar "+") x) y
>>> sub x y = EAp (EAp (EVar "-") x) y
>>> mul x y = EAp (EAp (EVar "*") x) y
>>> div x y = EAp (EAp (EVar "/") x) y
>>> eq x y = EAp (EAp (EVar "==") x) y
>>> ne x y = EAp (EAp (EVar "/=") x) y
>>> gt x y = EAp (EAp (EVar ">") x) y
>>> lt x y = EAp (EAp (EVar "<") x) y
>>> ge x y = EAp (EAp (EVar ">=") x) y
>>> le x y = EAp (EAp (EVar "<=") x) y
>>> ap f x = EAp f x
>>> and p q = EAp (EAp (EVar "&&") p) q
>>> or  p q = EAp (EAp (EVar "||") p) q
>>> printExpr = putStrLn . iDisplay . pprExpr Root defaultPrecAssoc
>>> printScDefn = putStrLn . iDisplay . pprScDefn
-}

{- |
>>> printExpr $ ((x `add` y) `add` z) `add` w
x + y + z + w

>>> printExpr $ (x `add` y) `add` (z `add` w)
x + y + z + w

>>> printExpr $ x `add` (y `add` (z `add` w))
x + y + z + w

>>> printExpr $ x `add` (y `add` z) `add` w
x + y + z + w

>>> printExpr $ ((x `sub` y) `sub` z) `sub` w
((x - y) - z) - w

>>> printExpr $ (x `sub` y) `sub` (z `sub` w)
(x - y) - (z - w)

>>> printExpr $ x `sub` (y `sub` (z `sub` w))
x - (y - (z - w))

>>> printExpr $ x `sub` (y `sub` z) `sub` w
(x - (y - z)) - w

>>> printExpr $ ((x `mul` y) `mul` z) `mul` w
x * y * z * w

>>> printExpr $ (x `mul` y) `mul` (z `mul` w)
x * y * z * w

>>> printExpr $ x `mul` (y `mul` (z `mul` w))
x * y * z * w

>>> printExpr $ x `mul` (y `mul` z) `mul` w
x * y * z * w

>>> printExpr $ ((x `div` y) `div` z) `div` w
((x / y) / z) / w

>>> printExpr $ (x `div` y) `div` (z `div` w)
(x / y) / (z / w)

>>> printExpr $ x `div` (y `div` (z `div` w))
x / (y / (z / w))

>>> printExpr $ x `div` (y `div` z) `div` w
(x / (y / z)) / w

>>> printExpr $ ((x `add` y) `sub` z) `add` w
((x + y) - z) + w

>>> printExpr $ (x `add` y) `sub` (z `add` w)
(x + y) - (z + w)

>>> printExpr $ x `add` (y `sub` (z `add` w))
x + (y - (z + w))

>>> printExpr $ x `add` (y `sub` z) `add` w
x + (y - z) + w

>>> printExpr $ ((x `sub` y) `add` z) `sub` w
((x - y) + z) - w

>>> printExpr $ (x `sub` y) `add` (z `sub` w)
(x - y) + (z - w)

>>> printExpr $ x `sub` (y `add` (z `sub` w))
x - (y + (z - w))

>>> printExpr $ x `sub` (y `add` z) `sub` w
(x - (y + z)) - w

>>> printExpr $ ((x `add` y) `mul` z) `add` w
(x + y) * z + w

>>> printExpr $ (x `add` y) `mul` (z `add` w)
(x + y) * (z + w)

>>> printExpr $ x `add` (y `mul` (z `add` w))
x + y * (z + w)

>>> printExpr $ x `add` (y `mul` z) `add` w
x + y * z + w

>>> printExpr $ ((x `mul` y) `add` z) `mul` w
(x * y + z) * w

>>> printExpr $ (x `mul` y) `add` (z `mul` w)
x * y + z * w

>>> printExpr $ x `mul` (y `add` (z `mul` w))
x * (y + z * w)

>>> printExpr $ x `mul` (y `add` z) `mul` w
x * (y + z) * w

>>> printExpr $ ((x `add` y) `div` z) `add` w
(x + y) / z + w

>>> printExpr $ (x `add` y) `div` (z `add` w)
(x + y) / (z + w)

>>> printExpr $ x `add` (y `div` (z `add` w))
x + y / (z + w)

>>> printExpr $ x `add` (y `div` z) `add` w
x + y / z + w

>>> printExpr $ ((x `div` y) `add` z) `div` w
(x / y + z) / w

>>> printExpr $ (x `div` y) `add` (z `div` w)
x / y + z / w

>>> printExpr $ x `div` (y `add` (z `div` w))
x / (y + z / w)

>>> printExpr $ x `div` (y `add` z) `div` w
(x / (y + z)) / w

>>> printExpr $ ((x `sub` y) `mul` z) `sub` w
(x - y) * z - w

>>> printExpr $ (x `sub` y) `mul` (z `sub` w)
(x - y) * (z - w)

>>> printExpr $ x `sub` (y `mul` (z `sub` w))
x - y * (z - w)

>>> printExpr $ x `sub` (y `mul` z) `sub` w
(x - y * z) - w

>>> printExpr $ ((x `mul` y) `sub` z) `mul` w
(x * y - z) * w

>>> printExpr $ (x `mul` y) `sub` (z `mul` w)
x * y - z * w

>>> printExpr $ x `mul` (y `sub` (z `mul` w))
x * (y - z * w)

>>> printExpr $ x `mul` (y `sub` z) `mul` w
x * (y - z) * w

>>> printExpr $ ((x `sub` y) `div` z) `sub` w
(x - y) / z - w

>>> printExpr $ (x `sub` y) `div` (z `sub` w)
(x - y) / (z - w)

>>> printExpr $ x `sub` (y `div` (z `sub` w))
x - y / (z - w)

>>> printExpr $ x `sub` (y `div` z) `sub` w
(x - y / z) - w

>>> printExpr $ ((x `div` y) `sub` z) `div` w
(x / y - z) / w

>>> printExpr $ (x `div` y) `sub` (z `div` w)
x / y - z / w

>>> printExpr $ x `div` (y `sub` (z `div` w))
x / (y - z / w)

>>> printExpr $ x `div` (y `sub` z) `div` w
(x / (y - z)) / w

>>> printExpr $ ((x `mul` y) `div` z) `mul` w
((x * y) / z) * w

>>> printExpr $ (x `mul` y) `div` (z `mul` w)
(x * y) / (z * w)

>>> printExpr $ x `mul` (y `div` (z `mul` w))
x * (y / (z * w))

>>> printExpr $ x `mul` (y `div` z) `mul` w
x * (y / z) * w

>>> printExpr $ ((x `div` y) `mul` z) `div` w
((x / y) * z) / w

>>> printExpr $ (x `div` y) `mul` (z `div` w)
(x / y) * (z / w)

>>> printExpr $ x `div` (y `mul` (z `div` w))
x / (y * (z / w))

>>> printExpr $ x `div` (y `mul` z) `div` w
(x / (y * z)) / w

>>> printExpr $ ((p `or` q) `and` r) `or` s
(p || q) && r || s

>>> printExpr $ (p `or` q) `and` (r `or` s)
(p || q) && (r || s)

>>> printExpr $ p `or` (q `and` (r `or` s))
p || q && (r || s)

>>> printExpr $ p `or` (q `and` r) `or` s
p || q && r || s

>>> printExpr $ ((p `and` q) `or` r) `and` s
(p && q || r) && s

>>> printExpr $ (p `and` q) `or` (r `and` s)
p && q || r && s

>>> printExpr $ p `and` (q `or` (r `and` s))
p && (q || r && s)

>>> printExpr $ p `and` (q `or` r) `and` s
p && (q || r) && s

>>> printExpr $ (x `add` y) `eq` (y `add` x)
x + y == y + x

>>> printExpr $ (x `sub` y) `ne` (y `sub` x)
x - y /= y - x

>>> printExpr $ (x `mul` y) `eq` (y `mul` x)
x * y == y * x

>>> printExpr $ (x `div` y) `ne` (y `div` x)
x / y /= y / x

>>> printExpr $ (h `ap` f `ap` g) `ap` (g `ap` x `ap` y) `ap` (f `ap` z)
h f g (g x y) (f z)

>>> printExpr $ x `add` (ELet nonRecursive [("y", x `add` x)] y)
x + (let
  y = x + x
in y)

>>> printExpr $ ELet nonRecursive [("y", ELet nonRecursive [("z", x `add` _1)] z)] (y `add` _2)
let
  y = let
        z = x + 1
      in z
in y + 2

>>> printExpr $ ELet recursive [("y", ELet recursive [("z", x `sub` _2)] z)] (y `mul` _3)
letrec
  y = letrec
        z = x - 2
      in z
in y * 3

>>> let xs = EVar "xs"
>>> let letExpr1 = ELet nonRecursive [("z", x `add` _1)] (z `mul` x)
>>> let letExpr2 = ELet recursive [("y", x `add` _2)] (y `add` x)
>>> printExpr $ ECase xs [(1, [], letExpr1), (2, ["y", "ys"], letExpr2)]
case xs of
  <1> -> let
           z = x + 1
         in z * x ;
  <2> y ys -> letrec
                y = x + 2
              in y + x

>>> let (xs, ys) = (EVar "xs", EVar "ys")
>>> let caseExpr1 = ECase xs [(1, [], x), (2, ["y", "ys"], y `add` _2)]
>>> let caseExpr2 = ECase ys [(1, [], y), (2, ["x", "xs"], x `mul` _3)]
>>> printExpr $ ELet recursive [("x", caseExpr1), ("y", caseExpr2)] (x `add` y)
letrec
  x = case xs of
        <1> -> x ;
        <2> y ys -> y + 2;
  y = case ys of
        <1> -> y ;
        <2> x xs -> x * 3
in x + y

>>> let (xs, ys, sum) = (EVar "xs", EVar "ys", EVar "sum")
>>> printExpr $ x `mul` (ECase xs [(1, [], x), (2, ["y", "ys"], y `add` (sum `ap` ys))])
x * (case xs of
  <1> -> x ;
  <2> y ys -> y + sum ys)
-}
pprExpr :: Level -> PrecAssoc -> CoreExpr -> Iseqrep
pprExpr _ _ (ENum n) = iStr (show n)
pprExpr _ _ (EVar v) = iStr v
pprExpr _ _ (EConstr tag arity)
  = iConcat $ map iStr ["Pack{", show tag, ",", show arity, "}"]
pprExpr _ pa (EAp (EAp (EVar op) e1) e2)
  | infixOperator op = if weakp pa' (prec pa) (assoc pa) then iParen e else e
  where e = iConcat [ pprExpr Sub pa' e1
                    , iSpace, iStr op, iSpace
                    , pprExpr Sub pa' e2
                    ]
        pa' = precAssoc op
pprExpr _ pa (EAp e1 e2)
  = if weakp pa (prec pa) (assoc pa) then iParen e else e
  where e = iConcat [ pprExpr Sub functionPrecAssoc e1
                    , iSpace
                    , pprExpr Sub functionArgPrecAssoc e2
                    ]
pprExpr l _ (ELet isrec defns expr)
  = if l /= Root then iParen e else e
  where
    keyword | not isrec = "let"
            | isrec     = "letrec"
    e = iConcat [ iStr keyword, iNewline
                , iStr "  ", iIndent (pprDefns defns), iNewline
                , iStr "in ", pprExpr Root defaultPrecAssoc expr
                ]
pprExpr l _ (ECase expr alts)
  = if l /= Root then iParen e else e
  where e = iConcat [ iStr "case ", iIndent (pprExpr Root defaultPrecAssoc expr), iStr " of", iNewline
                    , iStr "  ", iIndent (iInterleave iNL (map pprAlt alts))
                    ]
pprExpr l _ (ELam args expr)
  = if l /= Root then iParen e else e
  where e = iConcat [ iStr "\\ ", pprArgs args, iStr " -> "
                    , iIndent (pprExpr Root defaultPrecAssoc expr)
                    ]
iNL :: Iseq iseq => iseq
iNL = iSpace `iAppend` iStr ";" `iAppend` iNewline

pprAlt :: CoreAlter -> Iseqrep
pprAlt (i, args, expr)
  = iConcat [ iStr "<", iStr (show i), iStr ">", sep, pprArgs args
            , iStr " -> ", iIndent (pprExpr Root defaultPrecAssoc expr)
            ]
    where sep = if null args then iNil else iSpace

pprDefns :: [(Name, CoreExpr)] -> Iseqrep
pprDefns defns = iInterleave sep (map pprDefn defns)
  where
    sep = iConcat [ iStr ";", iNewline ]

pprDefn :: (Name, CoreExpr) -> Iseqrep
pprDefn (name, expr)
  = iConcat [ iStr name, iStr " = ", iIndent (pprExpr Root defaultPrecAssoc expr) ]

iConcat :: Iseq iseq => [iseq] -> iseq
iConcat = foldr iAppend iNil

iInterleave :: Iseq iseq => iseq -> [iseq] -> iseq
iInterleave sep [] = iNil
iInterleave sep [x] = x
iInterleave sep (x:xs)
  = x `iAppend` sep `iAppend` iInterleave sep xs


----------------------------------------------------------------------------------------
-- sample code and prelude
----------------------------------------------------------------------------------------

sampleProgram :: CoreProgram
sampleProgram
  = [ ("main", [], ap double _42)
    , ("double", ["x"], add x x)
    , ("f", ["x"]
      , ELet recursive
        [("y", add x _1)
        ,("z", add y _2)
        ]
        z)
      -- g = x + y > p * length xs
    , ("g", [], (x `add` y) `gt` (p `mul` (length `ap` xs)))
    ]
  where
    [x, y, z, f, g, p, xs, double, length] = map EVar ["x", "y", "z", "f", "g", "p", "xs", "double", "length"]
    [_1, _2, _3, _42] = map ENum [1, 2, 3, 42]
    add x y = EAp (EAp (EVar "+") x) y
    mul x y = EAp (EAp (EVar "*") x) y
    gt x y = EAp (EAp (EVar ">") x) y
    ap f x = EAp f x

preludeCode :: String
preludeCode
  = unlines [ "I x = x ;"
            , "K x y = x ;"
            , "K1 x y = y ;"
            , "S f g x = f x (g x) ;"
            , "compose f g x = f (g x) ;"
            , "twice f = compose f f"
            ]

preludeDefs :: CoreProgram
preludeDefs
  = [ ("I", ["x"], EVar "x")
    , ("K", ["x", "y"], EVar "x")
    , ("K1", ["x", "y"], EVar "y")
    , ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x"))
                             (EAp (EVar "g") (EVar "x")))
    , ("compose", ["f", "g", "x"], EAp (EVar "f")
                                   (EAp (EVar "g") (EVar "x")))
    , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
    ]
