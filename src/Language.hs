{-# LANGUAGE NPlusKPatterns #-}
module Language where

import Data.Char (isAlpha, isDigit)
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

type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

type CoreExpr = Expr Name
type Name = String
type IsRec = Bool

type Alter a = (Int, [a], Expr a)
type CoreAlter = Alter Name

recursive :: IsRec
recursive = True
nonRecursive :: IsRec
nonRecursive = False

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [ name | (name, rhs) <- defns ]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [ rhs | (name, rhs) <- defns ]

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

----------------------------------------------------------------------------------------
-- pretty printer
----------------------------------------------------------------------------------------

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)

-- | 抽象データ型的に使う
class Iseq iseq where
  iNil :: iseq
  iStr :: String -> iseq
  iAppend :: iseq -> iseq -> iseq
  iNewline :: iseq
  iIndent :: iseq -> iseq
  iDisplay :: iseq -> String

infixr 5 `iAppend`

-- | Iseq 抽象データ型の具体型
data Iseqrep = INil
             | IStr String
             | IAppend Iseqrep Iseqrep
             | IIndent Iseqrep
             | INewline
             deriving Show

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

iNum :: Iseq iseq => Int -> iseq
iNum n = iStr (show n)

iFWNum :: Iseq iseq => Int -> Int -> iseq
iFWNum width n = iStr (space (width - length digits) ++ digits)
  where digits = show n

iLayn :: Iseq iseq => [iseq] -> iseq
iLayn seqs = iConcat (zipWith lay_item [1..] seqs)
  where lay_item n seq = iConcat [iFWNum 4 n, iStr ") ", iIndent seq, iNewline]

iParen :: Iseq iseq => iseq -> iseq
iParen seq = iStr "(" `iAppend` seq `iAppend` iStr ")"

iSpace :: Iseq iseq => iseq
iSpace = iStr " "

iNL :: Iseq iseq => iseq
iNL = iStr ";" `iAppend` iNewline

iNL' :: Iseq iseq => iseq
iNL' = iSpace `iAppend` iNL

iConcat :: Iseq iseq => [iseq] -> iseq
iConcat = foldr iAppend iNil

iInterleave :: Iseq iseq => iseq -> [iseq] -> iseq
iInterleave sep []     = iNil
iInterleave sep [x]    = x
iInterleave sep (x:xs) = iConcat [x, sep, iInterleave sep xs]

type Precedence = Int
data Associativity = Infix | InfixL | InfixR deriving (Show, Enum, Eq)
-- | PrecAssoc
--
-- 下位の部分式に対して上位側からどういう優先度/結合性の中で call されているかを教える
-- 下位の式を pprExpr するところで判断してカッコを付けるかどうかなど決める
data PrecAssoc = PrecAssoc { weakp :: Precedence -> Associativity -> Bool
                           , prec  :: Precedence
                           , assoc :: Associativity
                           }

-- | 右辺値の最上位の式か下位の式か
data Level = Top | Sub deriving (Eq, Show)

space :: Int -> String
space n = replicate n ' '

{- |
>>> double = EVar "double"
>>> _42 = ENum 42

>>> printScDefn ("double", ["x"], x `add` x)
double x = x + x

>>> printScDefn ("main", [], ap double _42)
main = double 42

>>> printScDefn ("f", ["x"], ELet nonRecursive [("y", x `add` _1), ("z", y `mul` _2)] z)
f x = let
        y = x + 1;
        z = y * 2
      in z

>>> let func = ELam ["f", "i"] (EVar "bool" `ap` (i `mul` (f `ap` (i `sub` _1))) `ap` _1 `ap` (i `eq` _1))
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
pprProgram scdefns = iInterleave iNL' $ map pprScDefn scdefns

pprScDefn :: CoreScDefn -> Iseqrep
pprScDefn (name, args, expr)
  = iConcat [ iStr name, sep, pprArgs args
            , iStr " = "
            , iIndent (pprExpr Top defaultPrecAssoc expr)
            ]
    where sep = if null args then iNil else iSpace

pprArgs :: [String] -> Iseqrep
pprArgs args = iInterleave iSpace $ map iStr args

{- $setup
>>> [a, b, c, i, j, k, n, f, g, h, x, y, z, w, p, q, r, s] = map (EVar . (:[])) "abcijknfghxyzwpqrs"
>>> [xs, ys, zs] = map EVar ["xs", "ys", "zs"]
>>> [sum, length] = map EVar ["sum", "length"]
>>> [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9] = map ENum [0 .. 9]
>>> add x y = EAp (EAp (EVar "+") x) y
>>> sub x y = EAp (EAp (EVar "-") x) y
>>> mul x y = EAp (EAp (EVar "*") x) y
>>> div x y = EAp (EAp (EVar "/") x) y
>>> inc x   = x `add` _1
>>> dec x   = x `sub` _1
>>> eq x y  = EAp (EAp (EVar "==") x) y
>>> ne x y  = EAp (EAp (EVar "/=") x) y
>>> gt x y  = EAp (EAp (EVar ">") x) y
>>> lt x y  = EAp (EAp (EVar "<") x) y
>>> ge x y  = EAp (EAp (EVar ">=") x) y
>>> le x y  = EAp (EAp (EVar "<=") x) y
>>> ap f x  = EAp f x
>>> and p q = EAp (EAp (EVar "&&") p) q
>>> or  p q = EAp (EAp (EVar "||") p) q
>>> printExpr = putStrLn . iDisplay . pprExpr Top defaultPrecAssoc
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

>>> printExpr $ f `ap` x `ap` (g `ap` x)
f x (g x)

>>> printExpr $ f `ap` (g `ap` (h `ap` x))
f (g (h x))

>>> printExpr $ f `ap` (g `ap` x) `ap` (h `ap` y)
f (g x) (h y)

>>> printExpr $ x `add` (ELet nonRecursive [("y", x `add` _1), ("z", y `mul` _2)] z)
x + (let
  y = x + 1;
  z = y * 2
in z)

>>> printExpr $ (ELet nonRecursive [("y", x `add` _1), ("z", y `mul` _2)] z) `sub` x
(let
  y = x + 1;
  z = y * 2
in z) - x

>>> printExpr $ x `mul` (ECase xs [(1, [], x), (2, ["y", "ys"], y `add` (sum `ap` ys))])
x * (case xs of
  <1> -> x ;
  <2> y ys -> y + sum ys)

>>> printExpr $ (ECase xs [(1, [], x), (2, ["y", "ys"], y `add` (length `ap` ys))]) `div` x
(case xs of
  <1> -> x ;
  <2> y ys -> y + length ys) / x

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

>>> printExpr $ ELam ["x"] (x `ap` (y `ap` x))
\ x -> x (y x)

>>> printExpr $ ELam ["f"] (ELam ["x"] (f `ap` x `ap` x))
\ f -> \ x -> f x x

>>> let func = ELam ["f", "i"] (EVar "bool" `ap` (i `mul` (f `ap` (i `sub` _1))) `ap` _1 `ap` (i `eq` _1))
>>> printExpr $ ELet recursive [("y", ELam ["x"] (x `ap` (y `ap` x)))] (y `ap` func)
letrec
  y = \ x -> x (y x)
in y (\ f i -> bool (i * f (i - 1)) 1 (i == 1))
-}
pprExpr :: Level -> PrecAssoc -> CoreExpr -> Iseqrep
pprExpr _ _ (ENum n) = iNum n
pprExpr _ _ (EVar v) = iStr v
pprExpr _ _ (EConstr tag arity) = iConcat $ map iStr ["Pack{", show tag, ",", show arity, "}"]
pprExpr _ pa (EAp (EAp (EVar op) e1) e2)
  | infixOperator op = if weakp pa' (prec pa) (assoc pa) then iParen e else e
  where e = iConcat [ pprExpr Sub pa' e1
                    , iSpace, iStr op, iSpace
                    , pprExpr Sub pa' e2
                    ]
        pa' = precAssoc op
pprExpr _ pa (EAp e1 e2) = if weakp pa (prec pa) (assoc pa) then iParen e else e
  where e = iConcat [ pprExpr Sub functionPrecAssoc e1
                    , iSpace
                    , pprExpr Sub functionArgPrecAssoc e2
                    ]
pprExpr l _ (ELet isrec defns expr) = if l /= Top then iParen e else e
  where
    keyword | not isrec = "let"
            | isrec     = "letrec"
    e = iConcat [ iStr keyword, iNewline
                , iStr "  ", iIndent (pprDefns defns), iNewline
                , iStr "in ", pprExpr Top defaultPrecAssoc expr
                ]
pprExpr l _ (ECase expr alts) = if l /= Top then iParen e else e
  where e = iConcat [ iStr "case ", iIndent (pprExpr Top defaultPrecAssoc expr), iStr " of", iNewline
                    , iStr "  ", iIndent (iInterleave iNL' (map pprAlt alts))
                    ]
pprExpr l _ (ELam args expr) = if l /= Top then iParen e else e
  where e = iConcat [ iStr "\\ ", pprArgs args, iStr " -> "
                    , iIndent (pprExpr Top defaultPrecAssoc expr)
                    ]

precAssoc :: String -> PrecAssoc
precAssoc "*"  = PrecAssoc { weakp = \p a -> p >  5 || p == 5 && a /= InfixR, prec = 5, assoc = InfixR }
precAssoc "/"  = PrecAssoc { weakp = \p a -> p >= 5,                          prec = 5, assoc = Infix  }
precAssoc "+"  = PrecAssoc { weakp = \p a -> p >  4 || p == 4 && a /= InfixR, prec = 4, assoc = InfixR }
precAssoc "-"  = PrecAssoc { weakp = \p a -> p >= 4,                          prec = 4, assoc = Infix  }
precAssoc "==" = PrecAssoc { weakp = \p a -> p >  3,                          prec = 3, assoc = Infix  }
precAssoc "/=" = PrecAssoc { weakp = \p a -> p >  3,                          prec = 3, assoc = Infix  }
precAssoc ">"  = PrecAssoc { weakp = \p a -> p >  3,                          prec = 3, assoc = Infix  }
precAssoc ">=" = PrecAssoc { weakp = \p a -> p >  3,                          prec = 3, assoc = Infix  }
precAssoc "<"  = PrecAssoc { weakp = \p a -> p >  3,                          prec = 3, assoc = Infix  }
precAssoc "<=" = PrecAssoc { weakp = \p a -> p >  3,                          prec = 3, assoc = Infix  }
precAssoc "&&" = PrecAssoc { weakp = \p a -> p >  2,                          prec = 2, assoc = InfixR }
precAssoc "||" = PrecAssoc { weakp = \p a -> p >  1,                          prec = 1, assoc = InfixR }
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

pprDefns :: [(Name, CoreExpr)] -> Iseqrep
pprDefns defns = iInterleave iNL (map pprDefn defns)

pprDefn :: (Name, CoreExpr) -> Iseqrep
pprDefn (name, expr)
  = iConcat [ iStr name, iStr " = ", iIndent (pprExpr Top defaultPrecAssoc expr) ]

pprAlt :: CoreAlter -> Iseqrep
pprAlt (i, args, expr)
  = iConcat [ iStr "<", iStr (show i), iStr ">", sep, pprArgs args
            , iStr " -> ", iIndent (pprExpr Top defaultPrecAssoc expr)
            ]
    where sep = if null args then iNil else iSpace


----------------------------------------------------------------------------------------
-- parser
----------------------------------------------------------------------------------------

type Location = Int
type Token = (Location, String)
type Parser a = [Token] -> [(a, [Token])]

pSat :: (String -> Bool) -> Parser String
pSat pred ((_, tok):toks)
  | pred tok  = [(tok, toks)]
  | otherwise = []
pSat _ []     = []


{- |
>>> pLit "a" []
[]

>>> pLit "a" [(1,"a")]
[("a",[])]

>>> pLit "a" [(1,"b")]
[]

>>> pLit "a" [(1,"b"),(1,"a")]
[]
-}
pLit :: String -> Parser String
pLit s = pSat (s==)

{- |
>>> pVar []
[]

>>> pVar [(1, "a")]
[("a",[])]

>>> pVar [(1, "a"), (1, "b")]
[("a",[(1,"b")])]

>>> pVar [(1, "42")]
[]

>>> pVar [(1, "a42")]
[("a42",[])]

>>> pVar [(1, "let")]
[]

>>> pVar [(1, "letrec")]
[]

>>> pVar [(1, "in")]
[]

>>> pVar [(1, "case")]
[]

>>> pVar [(1, "of")]
[]

>>> pVar [(1, "Pack")]
[]

-}
pVar :: Parser String
pVar = pSat p
  where p cs@(c:_) = cs `notElem` keywords && isAlpha c

keywords :: [String]
keywords = ["let", "letrec", "in", "case", "of", "Pack"]

{- |
>>> pNum []
[]

>>> pNum [(1, "a")]
[]

>>> pNum [(1, "42")]
[(42,[])]

>>> pNum [(1, "4a")]
[]
-}
pNum :: Parser Int
pNum = pSat (all isDigit) `pApply` read


{- |
>>> pLit "Hello" `pAlt` pLit "Bye" $ []
[]

>>> pLit "Hello" `pAlt` pLit "Bye" $ [(1,"a")]
[]

>>> pLit "Hello" `pAlt` pLit "Bye" $ [(1,"Hello")]
[("Hello",[])]

>>> pLit "Hello" `pAlt` pLit "Bye" $ [(1,"Bye")]
[("Bye",[])]

>>> pLit "Hello" `pAlt` pLit "Bye" $ [(1,"Hellow")]
[]

>>> pLit "Hello" `pAlt` pLit "Bye" $ [(1,"Hellow"),(1,"Bye")]
[]

>>> pLit "Hello" `pAlt` pLit "Bye" $ [(1,"Bye"),(1,"Hello")]
[("Bye",[(1,"Hello")])]

-}
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks

{- |
>>> pThen (++) (pLit "Hello") (pLit "World") []
[]

>>> pThen (++) (pLit "Hello") (pLit "World") [(1, "World")]
[]

>>> pThen (++) (pLit "Hello") (pLit "World") [(1, "Hello"), (1, "World")]
[("HelloWorld",[])]
-}
pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks
  = [ (combine v1 v2, toks2)
    | (v1, toks1) <- p1 toks
    , (v2, toks2) <- p2 toks1
    ]

{- |
>>> pThen3 (\x y z -> x ++ y ++ z) (pLit "Hello ") pVar (pLit " san") []
[]

>>> pThen3 (\x y z -> x ++ y ++ z) (pLit "Hello ") pVar (pLit " san") [(1, "Hello "), (1, "cutsea"), (1, " san")]
[("Hello cutsea san",[])]

>>> pThen3 (\x y z -> x ++ y ++ z) (pLit "Hello ") pVar (pLit " san") [(1, "Hello "), (1, "cutsea"), (1, " kun")]
[]
-}
pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks
  = [ (v2, toks2)
    | (v1, toks1) <- p1 toks
    , (v2, toks2) <- pThen (combine v1) p2 p3 toks1
    ]

{- |
>>> pThen4 (,,,) pVar pVar pVar pVar []
[]

>>> pThen4 (,,,) pVar pVar pVar pVar [(1, "a"), (1, "b")]
[]

>>> pThen4 (,,,) pVar pVar pVar pVar [(1, "a"), (1, "b"), (1, "c")]
[]

>>> pThen4 (,,,) pVar pVar pVar pVar [(1, "a"), (1, "b"), (1, "c"), (1, "d")]
[(("a","b","c","d"),[])]

>>> pThen4 (,,,) pVar pVar pVar pVar [(1, "a"), (1, "b"), (1, "c"), (1, "d"), (1, "e")]
[(("a","b","c","d"),[(1,"e")])]
-}
pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 toks
  = [ (v2, toks2)
    | (v1, toks1) <- p1 toks
    , (v2, toks2) <- pThen3 (combine v1) p2 p3 p4 toks1
    ]
{--
-- COMPILE ERROR
pThenN combine [p1,p2] toks
  = [ (combine v1 v2, toks2)
    | (v1, toks1) <- p1 toks
    , (v2, toks2) <- p2 toks1
    ]
pThenN combine (p:ps)  toks
  = [ (v2, toks2)
    | (v1, toks1) <- p toks
    , (v2, toks2) <- pThenN (combine v1) ps toks1
    ]
--}

{- |
>>> pZeroOrMore (pLit "Hello") [(1, "Hello"), (1, "Bye")]
[(["Hello"],[(1,"Bye")]),([],[(1,"Hello"),(1,"Bye")])]

>>> pZeroOrMore (pLit "Hello") [(1, "Hellow"), (1, "Bye~")]
[([],[(1,"Hellow"),(1,"Bye~")])]
-}
pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pAlt` pEmpty []

{- |
>>> pEmpty [] $ []
[([],[])]

>>> pEmpty 1 $ []
[(1,[])]

>>> pEmpty "hello" $ [(1, "a")]
[("hello",[(1,"a")])]
-}
pEmpty :: a -> Parser a
pEmpty x toks = [(x, toks)]

{- |
>>> pOneOrMore (pLit "Hello") [(1, "Hello"), (1, "Bye")]
[(["Hello"],[(1,"Bye")])]

>>> pOneOrMore (pLit "Hello") [(1, "Hellow"), (1, "Bye~")]
[]
-}
pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p toks
  = [ (v1:vs, toks2)
    | (v1, toks1) <- p toks
    , (vs, toks2) <- pZeroOrMore p toks1
    ]

{- |
>>> pVar `pApply` (++"!") $ [(1, "a"), (1, "b"), (1, "c")]
[("a!",[(1,"b"),(1,"c")])]

>>> (pOneOrMore pVar) `pApply` (map (++"!")) $ [(1, "a"), (1, "b"), (1, "c")]
[(["a!","b!","c!"],[]),(["a!","b!"],[(1,"c")]),(["a!"],[(1,"b"),(1,"c")])]
-}
pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks = [(f v1, toks1) | (v1, toks1) <- p toks]

{- |
>>> pConst (pLit "a") (pLit "b") []
[]

>>> pConst (pLit "a") (pLit "b") [(1, "a")]
[]

>>> pConst (pLit "a") (pLit "b") [(1, "b")]
[]

>>> pConst (pLit "a") (pLit "b") [(1, "a"), (1, "b")]
[("a",[])]

>>> pConst (pLit "a") (pLit "b") [(1, "a"), (1, "b"),(1, "c")]
[("a",[(1,"c")])]
-}
pConst :: Parser a -> Parser b -> Parser a
pConst = pThen const

{- |
>>> pConst2 (pLit "a") (pLit "b") []
[]

>>> pConst2 (pLit "a") (pLit "b") [(1, "a")]
[]

>>> pConst2 (pLit "a") (pLit "b") [(1, "b")]
[]

>>> pConst2 (pLit "a") (pLit "b") [(1, "a"), (1, "b")]
[("b",[])]

>>> pConst2 (pLit "a") (pLit "b") [(1, "a"), (1, "b"), (1, "c")]
[("b",[(1,"c")])]
-}
pConst2 :: Parser a -> Parser b -> Parser b
pConst2 = pThen (flip const)

{- |
>>> pOneOrMoreWithSep pVar (pLit ",") []
[]

>>> pOneOrMoreWithSep pVar (pLit ",") [(1, "a")]
[(["a"],[])]

>>> pOneOrMoreWithSep pVar (pLit ",") [(1, "a"), (1, ",")]
[(["a"],[(1,",")])]

>>> pOneOrMoreWithSep pVar (pLit ",") [(1, "a"), (1, ","), (1, "b")]
[(["a","b"],[]),(["a"],[(1,","),(1,"b")])]

>>> pOneOrMoreWithSep pVar (pLit ",") [(1, "a"), (1, ","), (1, "b"), (1, ",")]
[(["a","b"],[(1,",")]),(["a"],[(1,","),(1,"b"),(1,",")])]

>>> pOneOrMoreWithSep pVar (pLit ",") [(1, "a"), (1, ","), (1, "b"), (1, ","), (1, "c")]
[(["a","b","c"],[]),(["a","b"],[(1,","),(1,"c")]),(["a"],[(1,","),(1,"b"),(1,","),(1,"c")])]

>>> pOneOrMoreWithSep pVar (pLit ",") [(1, "a"), (1, ","), (1, "b"), (1, "c"), (1, "d")]
[(["a","b"],[(1,"c"),(1,"d")]),(["a"],[(1,","),(1,"b"),(1,"c"),(1,"d")])]
-}
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p sep toks
  = [ (v1:vs, toks2)
    | (v1, toks1) <- p toks
    , (vs, toks2) <- pZeroOrMore (pConst2 sep p) toks1
    ]

pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = pLit "hello" `pAlt` pLit "goodbye"

{- |
>>> pGreeting [(1,"goodbye"), (1,"James"), (1,"!")]
[(("goodbye","James"),[])]
-}
pGreeting :: Parser (String, String)
pGreeting = pThen3 mkGreeting pHelloOrGoodbye pVar (pLit "!")
  where mkGreeting hg name _exclamation = (hg, name)

{- |
>>> clex 1 "123abc"
[(1,"123"),(1,"abc")]

>>> clex 1 "_x 42"
[(1,"_"),(1,"x"),(1,"42")]

>>> clex 1 "a_1 a_2 a_3"
[(1,"a_1"),(1,"a_2"),(1,"a_3")]

>>> clex 1 "Hello -- comment perapera ..\nWorld"
[(1,"Hello"),(2,"World")]

>>> clex 1 "x == y"
[(1,"x"),(1,"=="),(1,"y")]

>>> clex 1 "x /= y"
[(1,"x"),(1,"/="),(1,"y")]

>>> clex 1 "x >= y"
[(1,"x"),(1,">="),(1,"y")]

>>> clex 1 "x <= y"
[(1,"x"),(1,"<="),(1,"y")]

>>> clex 1 "x -> y"
[(1,"x"),(1,"->"),(1,"y")]

>>> clex 1 "red pink\nblue green\nyellow"
[(1,"red"),(1,"pink"),(2,"blue"),(2,"green"),(3,"yellow")]
-}
clex :: Int -> String -> [Token]
clex n ('\n':cs) = clex (n+1) cs
clex n ('-':'-':cs) = case dropWhile (/='\n') cs of
  [] -> []
  cs -> clex n cs
clex n (c1:c2:cs)
  | [c1,c2] `elem` twoCharOps = (n, [c1,c2]) : clex n cs
clex n (c:cs)
  | isWhiteSpace c = clex n cs
  | isDigit c      = let (numCs, restCs) = span isDigit cs
                         numToken        = c : numCs
                     in (n, numToken) : clex n restCs
  | isAlpha c      = let (idCs, restCs) = span isIdChar cs
                         varToken       = c : idCs
                     in (n, varToken) : clex n restCs
  | otherwise      = (n, [c]) : clex n cs
clex n []          = []

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || c == '_'

twoCharOps :: [String]
twoCharOps = [ "==", "/=", ">=", "<=", "->", "&&", "||" ]

syntax :: [Token] -> CoreProgram
syntax = takeFirstParse . pProgram
  where
    takeFirstParse ((prog, []) : others) = prog
    takeFirstParse (parse      : others) = takeFirstParse others
    takeFirstParse other                 = error "syntax error"

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

{- |
>>> pSc $ clex 1 "x = 42"
[(("x",[],ENum 42),[])]

>>> pSc $ clex 1 "f x = x"
[(("f",["x"],EVar "x"),[])]
-}
pSc :: Parser CoreScDefn
pSc = pThen4 mkSc pVar (pZeroOrMore pVar) (pLit "=") pExpr

mkSc :: Name -> [Name] -> p -> CoreExpr -> CoreScDefn
mkSc name args _ expr = (name, args, expr)

{- |
>>> pConstr $ clex 1 "Pack{1,2}"
[((1,2),[])]
-}
pConstr :: Parser (Int, Int)
pConstr = pThen3 (\_ x _ -> x) pre body post
  where pre  = pThen (,) (pLit "Pack") (pLit "{")
        body = pThen3 (\t _ a -> (t, a)) pNum (pLit ",") pNum
        post = pLit "}"

{- |
>>> pBinding $ clex 1 "x = 3"
[(("x",ENum 3),[])]

>>> pBinding $ clex 1 "x = y"
[(("x",EVar "y"),[])]
-}
pBinding :: Parser (Name, CoreExpr)
pBinding = pThen3 (\v _ e -> (v, e)) pVar (pLit "=") pExpr

{- |
>>> pBindings $ clex 1 "y = x; z = y"
[([("y",EVar "x"),("z",EVar "y")],[]),([("y",EVar "x")],[(1,";"),(1,"z"),(1,"="),(1,"y")])]
-}
pBindings :: Parser [(Name, CoreExpr)]
pBindings = pOneOrMoreWithSep pBinding (pLit ";")

{- |
>>> pLet $ clex 1 "let x = 3 in x"
[(ELet False [("x",ENum 3)] (EVar "x"),[])]

>>> pLet $ clex 1 "let y = x;z = y in z"
[(ELet False [("y",EVar "x"),("z",EVar "y")] (EVar "z"),[])]

>>> pLet $ clex 1 "letrec x = x in x"
[(ELet True [("x",EVar "x")] (EVar "x"),[])]

>>> pLet $ clex 1 "letrec y = x;x = y in x"
[(ELet True [("y",EVar "x"),("x",EVar "y")] (EVar "x"),[])]
-}
pLet :: Parser CoreExpr
pLet = pThen4 f (pLet `pAlt` pLetrec) pBindings (pLit "in") pExpr
  where f isRec bindings _ expr = ELet isRec bindings expr
        pLet = pLit "let" `pApply` const False
        pLetrec = pLit "letrec" `pApply` const True

{- |
>>> pArgs $ clex 1 ""
[([],[])]

>>> pArgs $ clex 1 "x"
[(["x"],[]),([],[(1,"x")])]

>>> pArgs $ clex 1 "x y"
[(["x","y"],[]),(["x"],[(1,"y")]),([],[(1,"x"),(1,"y")])]

>>> pArgs $ clex 1 "x y z"
[(["x","y","z"],[]),(["x","y"],[(1,"z")]),(["x"],[(1,"y"),(1,"z")]),([],[(1,"x"),(1,"y"),(1,"z")])]
-}
pArgs :: Parser [Name]
pArgs = pZeroOrMore pVar

{- |
>>> pArm $ clex 1 "<1> -> 42"
[((1,[],ENum 42),[])]

>>> pArm $ clex 1 "<1> -> x"
[((1,[],EVar "x"),[])]
-}
pArm :: Parser (Alter Name)
pArm = pThen4 f pTag pArgs (pLit "->") pExpr
  where f tag args _ expr = (tag, args, expr)
        pTag  = pThen3 (\_ tag _ -> tag) (pLit "<") pNum (pLit ">")

{- |
>>> pArms $ clex 1 "<1> -> x"
[([(1,[],EVar "x")],[])]

>>> pArms $ clex 1 "<1> -> x; <2> -> y"
[([(1,[],EVar "x"),(2,[],EVar "y")],[]),([(1,[],EVar "x")],[(1,";"),(1,"<"),(1,"2"),(1,">"),(1,"->"),(1,"y")])]
-}
pArms :: Parser [Alter Name]
pArms = pOneOrMoreWithSep pArm (pLit ";")

{- |
>>> pCase $ clex 1 "case x of <1> -> 42; <2> -> x"
[(ECase (EVar "x") [(1,[],ENum 42),(2,[],EVar "x")],[]),(ECase (EVar "x") [(1,[],ENum 42)],[(1,";"),(1,"<"),(1,"2"),(1,">"),(1,"->"),(1,"x")])]
-}
pCase :: Parser CoreExpr
pCase = pThen4 f (pLit "case") pExpr (pLit "of") pArms
  where f _ expr _ alters = ECase expr alters

{- |
>>> pAexpr [(1, "42")]
[(ENum 42,[])]

>>> pAexpr [(1, "a")]
[(EVar "a",[])]

>>> pAexpr $ clex 1 "Pack{1,2}"
[(EConstr 1 2,[])]
-}
pAexpr :: Parser CoreExpr
pAexpr = pVar' `pAlt` pNum' `pAlt` pConstr' `pAlt` pParenExpr
  where
    pNum' = pNum `pApply` ENum
    pVar' = pVar `pApply` EVar
    pConstr' = pConstr `pApply` uncurry EConstr
    pParenExpr = pThen3 (\_ e _ -> e) (pLit "(") pExpr (pLit ")")

{- |
>>> pLam $ clex 1 "\\ -> x"
[(ELam [] (EVar "x"),[])]

>>> pLam $ clex 1 "\\ x -> x"
[(ELam ["x"] (EVar "x"),[])]

>>> pLam $ clex 1 "\\ m x -> x x"
[(ELam ["m","x"] (EAp (EVar "x") (EVar "x")),[]),(ELam ["m","x"] (EVar "x"),[(1,"x")])]
-}
pLam :: Parser CoreExpr
pLam = pThen4 f (pLit "\\") pArgs (pLit "->") pExpr
  where f _ args _ expr = ELam args expr

pExpr :: Parser CoreExpr
pExpr = pLet `pAlt` pCase `pAlt` pLam `pAlt` pExpr1

pExpr1 :: Parser CoreExpr
pExpr1 = pThen3 f pExpr2 (pLit "||" `pApply` EVar) pExpr1 `pAlt`
         pExpr2
  where f e1 op e2 = EAp (EAp op e1) e2

pExpr2 :: Parser CoreExpr
pExpr2 = pThen3 f pExpr3 (pLit "&&" `pApply` EVar) pExpr2 `pAlt`
         pExpr3
  where f e1 op e2 = EAp (EAp op e1) e2

pExpr3 :: Parser CoreExpr
pExpr3 = pThen3 f pExpr4 (pRelop `pApply` EVar) pExpr4 `pAlt`
         pExpr4
  where f e1 op e2 = EAp (EAp op e1) e2

pRelop :: Parser String
pRelop = pLit "==" `pAlt` pLit "/=" `pAlt`
         pLit "<"  `pAlt` pLit "<=" `pAlt`
         pLit ">"  `pAlt` pLit ">="

{- |
>>> pExpr4 $ clex 1 "x + y"
[(EAp (EAp (EVar "+") (EVar "x")) (EVar "y"),[]),(EVar "x",[(1,"+"),(1,"y")])]

>>> pExpr4 $ clex 1 "x - y"
[(EAp (EAp (EVar "-") (EVar "x")) (EVar "y"),[]),(EVar "x",[(1,"-"),(1,"y")])]

>>> pExpr4 $ clex 1 "x + y + z"
[(EAp (EAp (EVar "+") (EVar "x")) (EAp (EAp (EVar "+") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "+") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")]),(EVar "x",[(1,"+"),(1,"y"),(1,"+"),(1,"z")])]

>>> pExpr4 $ clex 1 "x + (y + z)"
[(EAp (EAp (EVar "+") (EVar "x")) (EAp (EAp (EVar "+") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"+"),(1,"("),(1,"y"),(1,"+"),(1,"z"),(1,")")])]

>>> pExpr4 $ clex 1 "(x + y) + z"
[(EAp (EAp (EVar "+") (EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "+") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")])]

>>> pExpr4 $ clex 1 "x - y - z"
[(EAp (EAp (EVar "-") (EVar "x")) (EVar "y"),[(1,"-"),(1,"z")]),(EVar "x",[(1,"-"),(1,"y"),(1,"-"),(1,"z")])]

>>> pExpr4 $ clex 1 "x - (y - z)"
[(EAp (EAp (EVar "-") (EVar "x")) (EAp (EAp (EVar "-") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"-"),(1,"("),(1,"y"),(1,"-"),(1,"z"),(1,")")])]

>>> pExpr4 $ clex 1 "(x - y) - z"
[(EAp (EAp (EVar "-") (EAp (EAp (EVar "-") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "-") (EVar "x")) (EVar "y"),[(1,"-"),(1,"z")])]

>>> pExpr4 $ clex 1 "x + y - z"
[(EAp (EAp (EVar "+") (EVar "x")) (EAp (EAp (EVar "-") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "+") (EVar "x")) (EVar "y"),[(1,"-"),(1,"z")]),(EVar "x",[(1,"+"),(1,"y"),(1,"-"),(1,"z")])]

>>> pExpr4 $ clex 1 "x + (y - z)"
[(EAp (EAp (EVar "+") (EVar "x")) (EAp (EAp (EVar "-") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"+"),(1,"("),(1,"y"),(1,"-"),(1,"z"),(1,")")])]

>>> pExpr4 $ clex 1 "(x + y) - z"
[(EAp (EAp (EVar "-") (EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "+") (EVar "x")) (EVar "y"),[(1,"-"),(1,"z")])]

>>> pExpr4 $ clex 1 "x - y + z"
[(EAp (EAp (EVar "-") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")]),(EVar "x",[(1,"-"),(1,"y"),(1,"+"),(1,"z")])]

>>> pExpr4 $ clex 1 "x - (y + z)"
[(EAp (EAp (EVar "-") (EVar "x")) (EAp (EAp (EVar "+") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"-"),(1,"("),(1,"y"),(1,"+"),(1,"z"),(1,")")])]

>>> pExpr4 $ clex 1 "(x - y) + z"
[(EAp (EAp (EVar "+") (EAp (EAp (EVar "-") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "-") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")])]

>>> pExpr4 $ clex 1 "x + y * z"
[(EAp (EAp (EVar "+") (EVar "x")) (EAp (EAp (EVar "*") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "+") (EVar "x")) (EVar "y"),[(1,"*"),(1,"z")]),(EVar "x",[(1,"+"),(1,"y"),(1,"*"),(1,"z")])]

>>> pExpr4 $ clex 1 "x + (y * z)"
[(EAp (EAp (EVar "+") (EVar "x")) (EAp (EAp (EVar "*") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"+"),(1,"("),(1,"y"),(1,"*"),(1,"z"),(1,")")])]

>>> pExpr4 $ clex 1 "(x + y) * z"
[(EAp (EAp (EVar "*") (EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "+") (EVar "x")) (EVar "y"),[(1,"*"),(1,"z")])]

>>> pExpr4 $ clex 1 "x + y / z"
[(EAp (EAp (EVar "+") (EVar "x")) (EAp (EAp (EVar "/") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "+") (EVar "x")) (EVar "y"),[(1,"/"),(1,"z")]),(EVar "x",[(1,"+"),(1,"y"),(1,"/"),(1,"z")])]

>>> pExpr4 $ clex 1 "x + (y / z)"
[(EAp (EAp (EVar "+") (EVar "x")) (EAp (EAp (EVar "/") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"+"),(1,"("),(1,"y"),(1,"/"),(1,"z"),(1,")")])]

>>> pExpr4 $ clex 1 "(x + y) / z"
[(EAp (EAp (EVar "/") (EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "+") (EVar "x")) (EVar "y"),[(1,"/"),(1,"z")])]

>>> pExpr4 $ clex 1 "x * y + z"
[(EAp (EAp (EVar "+") (EAp (EAp (EVar "*") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "*") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")]),(EVar "x",[(1,"*"),(1,"y"),(1,"+"),(1,"z")])]

>>> pExpr4 $ clex 1 "x * (y + z)"
[(EAp (EAp (EVar "*") (EVar "x")) (EAp (EAp (EVar "+") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"*"),(1,"("),(1,"y"),(1,"+"),(1,"z"),(1,")")])]

>>> pExpr4 $ clex 1 "(x * y) + z"
[(EAp (EAp (EVar "+") (EAp (EAp (EVar "*") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "*") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")])]

>>> pExpr4 $ clex 1 "x / y + z"
[(EAp (EAp (EVar "+") (EAp (EAp (EVar "/") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "/") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")]),(EVar "x",[(1,"/"),(1,"y"),(1,"+"),(1,"z")])]

>>> pExpr4 $ clex 1 "x / (y + z)"
[(EAp (EAp (EVar "/") (EVar "x")) (EAp (EAp (EVar "+") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"/"),(1,"("),(1,"y"),(1,"+"),(1,"z"),(1,")")])]

>>> pExpr4 $ clex 1 "(x / y) + z"
[(EAp (EAp (EVar "+") (EAp (EAp (EVar "/") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "/") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")])]

>>> pExpr4 $ clex 1 "x - y * z"
[(EAp (EAp (EVar "-") (EVar "x")) (EAp (EAp (EVar "*") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "-") (EVar "x")) (EVar "y"),[(1,"*"),(1,"z")]),(EVar "x",[(1,"-"),(1,"y"),(1,"*"),(1,"z")])]

>>> pExpr4 $ clex 1 "x - (y * z)"
[(EAp (EAp (EVar "-") (EVar "x")) (EAp (EAp (EVar "*") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"-"),(1,"("),(1,"y"),(1,"*"),(1,"z"),(1,")")])]

>>> pExpr4 $ clex 1 "(x - y) * z"
[(EAp (EAp (EVar "*") (EAp (EAp (EVar "-") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "-") (EVar "x")) (EVar "y"),[(1,"*"),(1,"z")])]

>>> pExpr4 $ clex 1 "x - y / z"
[(EAp (EAp (EVar "-") (EVar "x")) (EAp (EAp (EVar "/") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "-") (EVar "x")) (EVar "y"),[(1,"/"),(1,"z")]),(EVar "x",[(1,"-"),(1,"y"),(1,"/"),(1,"z")])]

>>> pExpr4 $ clex 1 "x - (y / z)"
[(EAp (EAp (EVar "-") (EVar "x")) (EAp (EAp (EVar "/") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"-"),(1,"("),(1,"y"),(1,"/"),(1,"z"),(1,")")])]

>>> pExpr4 $ clex 1 "(x - y) / z"
[(EAp (EAp (EVar "/") (EAp (EAp (EVar "-") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "-") (EVar "x")) (EVar "y"),[(1,"/"),(1,"z")])]

>>> pExpr4 $ clex 1 "x * y - z"
[(EAp (EAp (EVar "-") (EAp (EAp (EVar "*") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "*") (EVar "x")) (EVar "y"),[(1,"-"),(1,"z")]),(EVar "x",[(1,"*"),(1,"y"),(1,"-"),(1,"z")])]

>>> pExpr4 $ clex 1 "x * (y - z)"
[(EAp (EAp (EVar "*") (EVar "x")) (EAp (EAp (EVar "-") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"*"),(1,"("),(1,"y"),(1,"-"),(1,"z"),(1,")")])]

>>> pExpr4 $ clex 1 "(x * y) - z"
[(EAp (EAp (EVar "-") (EAp (EAp (EVar "*") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "*") (EVar "x")) (EVar "y"),[(1,"-"),(1,"z")])]

>>> pExpr4 $ clex 1 "x / y - z"
[(EAp (EAp (EVar "-") (EAp (EAp (EVar "/") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "/") (EVar "x")) (EVar "y"),[(1,"-"),(1,"z")]),(EVar "x",[(1,"/"),(1,"y"),(1,"-"),(1,"z")])]

>>> pExpr4 $ clex 1 "x / (y - z)"
[(EAp (EAp (EVar "/") (EVar "x")) (EAp (EAp (EVar "-") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"/"),(1,"("),(1,"y"),(1,"-"),(1,"z"),(1,")")])]

>>> pExpr4 $ clex 1 "(x / y) - z"
[(EAp (EAp (EVar "-") (EAp (EAp (EVar "/") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "/") (EVar "x")) (EVar "y"),[(1,"-"),(1,"z")])]
-}
pExpr4 :: Parser CoreExpr
pExpr4 = pThen3 f pExpr5 (pLit "+" `pApply` EVar) pExpr4 `pAlt`
         pThen3 f pExpr5 (pLit "-" `pApply` EVar) pExpr5 `pAlt`
         pExpr5
  where f e1 op e2 = EAp (EAp op e1) e2

{- |
>>> pExpr5 $ clex 1 "x * y"
[(EAp (EAp (EVar "*") (EVar "x")) (EVar "y"),[]),(EVar "x",[(1,"*"),(1,"y")])]

>>> pExpr5 $ clex 1 "x / y"
[(EAp (EAp (EVar "/") (EVar "x")) (EVar "y"),[]),(EVar "x",[(1,"/"),(1,"y")])]

>>> pExpr5 $ clex 1 "x * y * z"
[(EAp (EAp (EVar "*") (EVar "x")) (EAp (EAp (EVar "*") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "*") (EVar "x")) (EVar "y"),[(1,"*"),(1,"z")]),(EVar "x",[(1,"*"),(1,"y"),(1,"*"),(1,"z")])]

>>> pExpr5 $ clex 1 "x * (y * z)"
[(EAp (EAp (EVar "*") (EVar "x")) (EAp (EAp (EVar "*") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"*"),(1,"("),(1,"y"),(1,"*"),(1,"z"),(1,")")])]

>>> pExpr5 $ clex 1 "(x * y) * z"
[(EAp (EAp (EVar "*") (EAp (EAp (EVar "*") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "*") (EVar "x")) (EVar "y"),[(1,"*"),(1,"z")])]

>>> pExpr5 $ clex 1 "x / y / z"
[(EAp (EAp (EVar "/") (EVar "x")) (EVar "y"),[(1,"/"),(1,"z")]),(EVar "x",[(1,"/"),(1,"y"),(1,"/"),(1,"z")])]

>>> pExpr5 $ clex 1 "x / (y / z)"
[(EAp (EAp (EVar "/") (EVar "x")) (EAp (EAp (EVar "/") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"/"),(1,"("),(1,"y"),(1,"/"),(1,"z"),(1,")")])]

>>> pExpr5 $ clex 1 "(x / y) / z"
[(EAp (EAp (EVar "/") (EAp (EAp (EVar "/") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "/") (EVar "x")) (EVar "y"),[(1,"/"),(1,"z")])]

>>> pExpr5 $ clex 1 "x * y / z"
[(EAp (EAp (EVar "*") (EVar "x")) (EAp (EAp (EVar "/") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "*") (EVar "x")) (EVar "y"),[(1,"/"),(1,"z")]),(EVar "x",[(1,"*"),(1,"y"),(1,"/"),(1,"z")])]

>>> pExpr5 $ clex 1 "x * (y / z)"
[(EAp (EAp (EVar "*") (EVar "x")) (EAp (EAp (EVar "/") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"*"),(1,"("),(1,"y"),(1,"/"),(1,"z"),(1,")")])]

>>> pExpr5 $ clex 1 "(x * y) / z"
[(EAp (EAp (EVar "/") (EAp (EAp (EVar "*") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "*") (EVar "x")) (EVar "y"),[(1,"/"),(1,"z")])]

>>> pExpr5 $ clex 1 "x / y * z"
[(EAp (EAp (EVar "/") (EVar "x")) (EVar "y"),[(1,"*"),(1,"z")]),(EVar "x",[(1,"/"),(1,"y"),(1,"*"),(1,"z")])]

>>> pExpr5 $ clex 1 "x / (y * z)"
[(EAp (EAp (EVar "/") (EVar "x")) (EAp (EAp (EVar "*") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"/"),(1,"("),(1,"y"),(1,"*"),(1,"z"),(1,")")])]

>>> pExpr5 $ clex 1 "(x / y) * z"
[(EAp (EAp (EVar "*") (EAp (EAp (EVar "/") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "/") (EVar "x")) (EVar "y"),[(1,"*"),(1,"z")])]
-}
pExpr5 :: Parser CoreExpr
pExpr5 = pThen3 f pExpr6 (pLit "*" `pApply` EVar) pExpr5 `pAlt`
         pThen3 f pExpr6 (pLit "/" `pApply` EVar) pExpr6 `pAlt`
         pExpr6
  where f e1 op e2 = EAp (EAp op e1) e2

{- |
>>> pExpr6 $ clex 1 "42"
[(ENum 42,[])]

>>> pExpr6 $ clex 1 "x"
[(EVar "x",[])]

>>> pExpr6 $ clex 1 "f x"
[(EAp (EVar "f") (EVar "x"),[]),(EVar "f",[(1,"x")])]

>>> pExpr6 $ clex 1 "f x y"
[(EAp (EAp (EVar "f") (EVar "x")) (EVar "y"),[]),(EAp (EVar "f") (EVar "x"),[(1,"y")]),(EVar "f",[(1,"x"),(1,"y")])]
-}
pExpr6 :: Parser CoreExpr
pExpr6 = pOneOrMore pAexpr `pApply` mkApChain

mkApChain :: [CoreExpr] -> CoreExpr
mkApChain = foldl1 EAp

parse :: String -> CoreProgram
parse = syntax . clex 1


----------------------------------------------------------------------------------------
-- sample code and prelude
----------------------------------------------------------------------------------------
testProgram :: String
testProgram = unlines [ "f = 3 ;"
                      , "g x y = let z = y in z ;"
                      , "h x = case (let y = x in y) of"
                      , "        <1> -> 2;"
                      , "        <2> -> 5"
                      ]

-- これは区別ができない
hangEleProblem :: String
hangEleProblem = unlines [ "f x y = case x of"
                         , "          <1> -> case y of"
                         , "                   <1> -> 1;"
                         , "          <2> -> 2"
                         ]

sampleProgram :: CoreProgram
sampleProgram
  = [ ("main", [], ap double _42)
    , ("double", ["x"], add x x)
      -- f x = let
      --         y = x + 1;
      --         z = y + 2
      --       in z
    , ("f", ["x"], ELet recursive [("y", x `add` _1), ("z", y `add` _2)] z)
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
