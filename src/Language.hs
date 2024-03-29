{-# LANGUAGE NPlusKPatterns #-}
module Language
  ( Expr (..)
  , Name
  , CoreAlt
  , CoreExpr
  , CoreScDefn
  , CoreProgram
  , IsRec
  , Tag
  , Arity
  , parse
  , preludeDefs
  -- ?
  , pConstr
  , clex
  ) where

import Data.Char (isAlpha, isDigit, isSpace)
import Text.Printf

import Iseq
import Parser

data Expr a
  = EVar Name
  | ENum Int
  | EConstr Tag Arity
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

type Tag = Int
type Arity = Int

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

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

iNL :: Iseq iseq => iseq
iNL = iStr ";" `iAppend` iNewline

iNL' :: Iseq iseq => iseq
iNL' = iSpace `iAppend` iNL

type Precedence = Int
data Associativity = Infix | InfixL | InfixR deriving (Show, Enum, Eq)
-- | Fixity
--
-- 下位の部分式に対して上位側からどういう優先度/結合性の中で call されているかを教える
-- 下位の式を pprExpr するところで判断してカッコを付けるかどうかなど決める
data Fixity = Fixity { weakp :: Precedence -> Associativity -> Bool
                     , prec  :: Precedence
                     , assoc :: Associativity
                     }

-- | 右辺値の最上位の式か下位の式か
data Level = Top | Sub deriving (Eq, Show)

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
pprProgram :: CoreProgram -> IseqRep
pprProgram scdefns = iInterleave iNL' $ map pprScDefn scdefns

pprScDefn :: CoreScDefn -> IseqRep
pprScDefn (name, args, expr)
  = iConcat [ iStr name, sep, pprArgs args
            , iStr " = "
            , iIndent (pprExpr Top defaultFixity expr)
            ]
    where sep = if null args then iNil else iSpace

pprArgs :: [String] -> IseqRep
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
>>> printExpr = putStrLn . iDisplay . pprExpr Top defaultFixity
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
pprExpr :: Level -> Fixity -> CoreExpr -> IseqRep
pprExpr _ _ (ENum n) = iNum n
pprExpr _ _ (EVar v) = iStr v
pprExpr _ _ (EConstr tag arity) = iConcat $ map iStr ["Pack{", show tag, ",", show arity, "}"]
pprExpr _ pa (EAp (EAp (EVar op) e1) e2)
  | infixOperator op = if weakp pa' (prec pa) (assoc pa) then iParen e else e
  where e = iConcat [ pprExpr Sub pa' e1
                    , iSpace, iStr op, iSpace
                    , pprExpr Sub pa' e2
                    ]
        pa' = fixity op
pprExpr _ pa (EAp e1 e2) = if weakp pa (prec pa) (assoc pa) then iParen e else e
  where e = iConcat [ pprExpr Sub functionFixity e1
                    , iSpace
                    , pprExpr Sub functionArgFixity e2
                    ]
pprExpr l _ (ELet isrec defns expr) = if l /= Top then iParen e else e
  where keyword = if isrec then "letrec" else "let"
        e = iConcat [ iStr keyword, iNewline
                    , iStr "  ", iIndent (pprDefns defns), iNewline
                    , iStr "in ", pprExpr Top defaultFixity expr
                    ]
pprExpr l _ (ECase expr alts) = if l /= Top then iParen e else e
  where e = iConcat [ iStr "case ", iIndent (pprExpr Top defaultFixity expr), iStr " of", iNewline
                    , iStr "  ", iIndent (iInterleave iNL' (map pprAlt alts))
                    ]
pprExpr l _ (ELam args expr) = if l /= Top then iParen e else e
  where e = iConcat [ iStr "\\ ", pprArgs args, iStr " -> "
                    , iIndent (pprExpr Top defaultFixity expr)
                    ]

fixity :: String -> Fixity
fixity "*"  = Fixity { weakp = \p a -> p >  5 || p == 5 && a /= InfixR, prec = 5, assoc = InfixR }
fixity "/"  = Fixity { weakp = \p a -> p >= 5,                          prec = 5, assoc = Infix  }
fixity "+"  = Fixity { weakp = \p a -> p >  4 || p == 4 && a /= InfixR, prec = 4, assoc = InfixR }
fixity "-"  = Fixity { weakp = \p a -> p >= 4,                          prec = 4, assoc = Infix  }
fixity "==" = Fixity { weakp = \p a -> p >  3,                          prec = 3, assoc = Infix  }
fixity "/=" = Fixity { weakp = \p a -> p >  3,                          prec = 3, assoc = Infix  }
fixity ">"  = Fixity { weakp = \p a -> p >  3,                          prec = 3, assoc = Infix  }
fixity ">=" = Fixity { weakp = \p a -> p >  3,                          prec = 3, assoc = Infix  }
fixity "<"  = Fixity { weakp = \p a -> p >  3,                          prec = 3, assoc = Infix  }
fixity "<=" = Fixity { weakp = \p a -> p >  3,                          prec = 3, assoc = Infix  }
fixity "&&" = Fixity { weakp = \p a -> p >  2 || p == 2 && a /= InfixR, prec = 2, assoc = InfixR }
fixity "||" = Fixity { weakp = \p a -> p >  1 || p == 1 && a /= InfixR, prec = 1, assoc = InfixR }
fixity _    = error "Unknown infix operator"

defaultFixity :: Fixity
defaultFixity = Fixity { weakp = \p a -> False, prec = 0, assoc = Infix }  -- FIXME!
functionFixity :: Fixity
functionFixity = Fixity { weakp = \p a -> p > 6, prec = 6, assoc = InfixL }
functionArgFixity :: Fixity
functionArgFixity = Fixity { weakp = \p a -> p >= 6, prec = 6, assoc = InfixL }

infixOperator :: String -> Bool
infixOperator op
  = op `elem` [ "*", "/"
              , "+", "-"
              , "==", "/="
              , ">", ">=", "<", "<="
              , "&&", "||"
              ]

pprDefns :: [(Name, CoreExpr)] -> IseqRep
pprDefns defns = iInterleave iNL (map pprDefn defns)

pprDefn :: (Name, CoreExpr) -> IseqRep
pprDefn (name, expr)
  = iConcat [ iStr name, iStr " = ", iIndent (pprExpr Top defaultFixity expr) ]

pprAlt :: CoreAlt -> IseqRep
pprAlt (i, args, expr)
  = iConcat [ iStr "<", iStr (show i), iStr ">", sep, pprArgs args
            , iStr " -> ", iIndent (pprExpr Top defaultFixity expr)
            ]
    where sep = if null args then iNil else iSpace


----------------------------------------------------------------------------------------
-- parser
----------------------------------------------------------------------------------------
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

clex :: Location -> String -> [Token]
clex n ('\n':cs) = clex (n+1) cs
clex n ('-':'-':cs) = case dropWhile (/='\n') cs of
  [] -> []
  cs -> clex n cs
clex n (c1:c2:cs)
  | [c1,c2] `elem` twoCharOps = (n, [c1,c2]) : clex n cs
clex n (c:cs)
  | isSpace c = clex n cs
  | isDigit c = let (numCs, restCs) = span isDigit cs
                    numToken        = c : numCs
                in (n, numToken) : clex n restCs
  | isAlpha c = let (idCs, restCs) = span isIdChar cs
                    varToken       = c : idCs
                in (n, varToken) : clex n restCs
  | otherwise = (n, [c]) : clex n cs
clex n []     = []

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || c == '_'

twoCharOps :: [String]
twoCharOps = [ "==", "/=", ">=", "<=", "->", "&&", "||" ]

{- |
>>> runParser pVar []
[]

>>> runParser pVar [(1, "a")]
[("a",[])]

>>> runParser pVar [(1, "a"), (1, "b")]
[("a",[(1,"b")])]

>>> runParser pVar [(1, "a1"), (1, "b1")]
[("a1",[(1,"b1")])]

>>> runParser pVar [(1, "42")]
[]

>>> runParser pVar [(1, "a42")]
[("a42",[])]

>>> runParser pVar [(1, "let")]
[]

>>> runParser pVar [(1, "letrec")]
[]

>>> runParser pVar [(1, "in")]
[]

>>> runParser pVar [(1, "case")]
[]

>>> runParser pVar [(1, "of")]
[]

>>> runParser pVar [(1, "Pack")]
[]

-}
pVar :: Parser String
pVar = pSat p
  where p cs@(c:_) = cs `notElem` keywords && isAlpha c

keywords :: [String]
keywords = ["let", "letrec", "in", "case", "of", "Pack"]

syntax :: String -> [Token] -> CoreProgram
syntax src = takeFirstParse . runParser pProgram
  where
    takeFirstParse ((prog, []):other) = prog
    takeFirstParse ((prog, xs):other) = error msg
      where (n, w):_ = xs
            msg = unlines [ "syntax error:"
                          , "-----------------"
                          , src'
                          , "-----------------"
                          , "source line " ++ show n ++ ": Got '" ++ w ++ "'"
                          ]
            src' = unlines $ zipWith showLine [1..] (lines src)
              where showLine :: Int -> String -> String
                    showLine no line
                      | no == n   = printf "🐛%05d| " no ++ line
                      | otherwise = printf "  %05d| " no ++ line
    takeFirstParse other              = error "syntax error"

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

{- |
>>> runParser pSc $ clex 1 "x = 42"
[(("x",[],ENum 42),[])]

>>> runParser pSc $ clex 1 "f x = x"
[(("f",["x"],EVar "x"),[])]
-}
pSc :: Parser CoreScDefn
pSc = mkSc <$> pVar <*> pZeroOrMore pVar <* pLit "=" <*> pExpr

mkSc :: Name -> [Name] -> CoreExpr -> CoreScDefn
mkSc = (,,)

{- |
>>> runParser pConstr $ clex 1 "Pack{1,2}"
[((1,2),[])]
-}
pConstr :: Parser (Tag, Arity)
pConstr = pLit "Pack" *> pLit "{" *> pTagArity <* pLit "}"

{- |
>>> runParser pTagArity $ clex 1 "1,2"
[((1,2),[])]
-}
pTagArity :: Parser (Tag, Arity)
pTagArity = (,) <$> pNum <* pLit "," <*> pNum

{- |
>>> runParser pBinding $ clex 1 "x = 3"
[(("x",ENum 3),[])]

>>> runParser pBinding $ clex 1 "x = y"
[(("x",EVar "y"),[])]
-}
pBinding :: Parser (Name, CoreExpr)
pBinding = (,) <$> pVar <* pLit "=" <*> pExpr

{- |
>>> runParser pBindings $ clex 1 "y = x; z = y"
[([("y",EVar "x"),("z",EVar "y")],[])]
-}
pBindings :: Parser [(Name, CoreExpr)]
pBindings = pOneOrMoreWithSep pBinding (pLit ";")

{- |
>>> runParser pLet $ clex 1 "let x = 3 in x"
[(ELet False [("x",ENum 3)] (EVar "x"),[])]

>>> runParser pLet $ clex 1 "let y = x;z = y in z"
[(ELet False [("y",EVar "x"),("z",EVar "y")] (EVar "z"),[])]

>>> runParser pLet $ clex 1 "letrec x = x in x"
[(ELet True [("x",EVar "x")] (EVar "x"),[])]

>>> runParser pLet $ clex 1 "letrec y = x;x = y in x"
[(ELet True [("y",EVar "x"),("x",EVar "y")] (EVar "x"),[])]
-}
pLet :: Parser CoreExpr
pLet = ELet <$> (pLetrec `pAlt` pLet) <*> pBindings <* pLit "in" <*> pExpr
  where pLetrec = True  <$ pLit "letrec"
        pLet    = False <$ pLit "let"

{- |
>>> runParser pArgs $ clex 1 ""
[([],[])]

>>> runParser pArgs $ clex 1 "x"
[(["x"],[]),([],[(1,"x")])]

>>> runParser pArgs $ clex 1 "x y"
[(["x","y"],[]),(["x"],[(1,"y")]),([],[(1,"x"),(1,"y")])]

>>> runParser pArgs $ clex 1 "x y z"
[(["x","y","z"],[]),(["x","y"],[(1,"z")]),(["x"],[(1,"y"),(1,"z")]),([],[(1,"x"),(1,"y"),(1,"z")])]
-}
pArgs :: Parser [Name]
pArgs = pZeroOrMore pVar

{- |
>>> runParser pArm $ clex 1 "<1> -> 42"
[((1,[],ENum 42),[])]

>>> runParser pArm $ clex 1 "<1> -> x"
[((1,[],EVar "x"),[])]
-}
pArm :: Parser (Alter Name)
pArm = (,,) <$> pTag <*> pArgs <* pLit "->" <*> pExpr
  where pTag = pLit "<" *> pNum <* pLit ">"

{- |
>>> runParser pArms $ clex 1 "<1> -> x"
[([(1,[],EVar "x")],[])]

>>> runParser pArms $ clex 1 "<1> -> x; <2> -> y"
[([(1,[],EVar "x"),(2,[],EVar "y")],[])]
-}
pArms :: Parser [Alter Name]
pArms = pOneOrMoreWithSep pArm (pLit ";")

{- |
>>> runParser pCase $ clex 1 "case x of <1> -> 42; <2> -> x"
[(ECase (EVar "x") [(1,[],ENum 42),(2,[],EVar "x")],[])]
-}
pCase :: Parser CoreExpr
pCase = ECase <$> (pLit "case" *> pExpr <* pLit "of") <*> pArms

{- |
>>> runParser pAexpr [(1, "42")]
[(ENum 42,[])]

>>> runParser pAexpr [(1, "a")]
[(EVar "a",[])]

>>> runParser pAexpr $ clex 1 "Pack{1,2}"
[(EConstr 1 2,[])]
-}
pAexpr :: Parser CoreExpr
pAexpr =
  (ENum <$> pNum) `pAlt`
  (EVar <$> pVar) `pAlt`
  uncurry EConstr <$> pConstr `pAlt`
  pLit "(" *> pExpr <* pLit ")"

{- |
>>> runParser pLam $ clex 1 "\\ -> x"
[(ELam [] (EVar "x"),[])]

>>> runParser pLam $ clex 1 "\\ x -> x"
[(ELam ["x"] (EVar "x"),[])]

>>> runParser pLam $ clex 1 "\\ m x -> x x"
[(ELam ["m","x"] (EAp (EVar "x") (EVar "x")),[]),(ELam ["m","x"] (EVar "x"),[(1,"x")])]
-}
pLam :: Parser CoreExpr
pLam = ELam <$> (pLit "\\" *> pArgs) <*> (pLit "->" *> pExpr)

data PartialExpr = NoOp | FoundOp Name CoreExpr

assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2

pExpr :: Parser CoreExpr
pExpr = pLet `pAlt` pCase `pAlt` pLam `pAlt` pExpr1

{- |
>>> runParser pExpr1 $ clex 1 "x || y"
[(EAp (EAp (EVar "||") (EVar "x")) (EVar "y"),[]),(EVar "x",[(1,"||"),(1,"y")])]

>>> runParser pExpr1 $ clex 1 "x || y && z"
[(EAp (EAp (EVar "||") (EVar "x")) (EAp (EAp (EVar "&&") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "||") (EVar "x")) (EVar "y"),[(1,"&&"),(1,"z")]),(EVar "x",[(1,"||"),(1,"y"),(1,"&&"),(1,"z")])]

>>> runParser pExpr1 $ clex 1 "x && y || z"
[(EAp (EAp (EVar "||") (EAp (EAp (EVar "&&") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "&&") (EVar "x")) (EVar "y"),[(1,"||"),(1,"z")]),(EVar "x",[(1,"&&"),(1,"y"),(1,"||"),(1,"z")])]

>>> runParser pExpr1 $ clex 1 "(x || y) && z"
[(EAp (EAp (EVar "&&") (EAp (EAp (EVar "||") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "||") (EVar "x")) (EVar "y"),[(1,"&&"),(1,"z")])]

>>> runParser pExpr1 $ clex 1 "x && (y || z)"
[(EAp (EAp (EVar "&&") (EVar "x")) (EAp (EAp (EVar "||") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"&&"),(1,"("),(1,"y"),(1,"||"),(1,"z"),(1,")")])]

>>> runParser pExpr1 $ clex 1 "x && (y && z)"
[(EAp (EAp (EVar "&&") (EVar "x")) (EAp (EAp (EVar "&&") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"&&"),(1,"("),(1,"y"),(1,"&&"),(1,"z"),(1,")")])]

>>> runParser pExpr1 $ clex 1 "(x && y) && z"
[(EAp (EAp (EVar "&&") (EAp (EAp (EVar "&&") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "&&") (EVar "x")) (EVar "y"),[(1,"&&"),(1,"z")])]

>>> runParser pExpr1 $ clex 1 "x || (y || z)"
[(EAp (EAp (EVar "||") (EVar "x")) (EAp (EAp (EVar "||") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"||"),(1,"("),(1,"y"),(1,"||"),(1,"z"),(1,")")])]

>>> runParser pExpr1 $ clex 1 "(x || y) || z"
[(EAp (EAp (EVar "||") (EAp (EAp (EVar "||") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "||") (EVar "x")) (EVar "y"),[(1,"||"),(1,"z")])]
-}
pExpr1 :: Parser CoreExpr
pExpr1 = assembleOp <$> pExpr2 <*> pExpr1c
{-
pExpr1 :: Parser CoreExpr
pExpr1 = pThen3 f pExpr2 (pLit "||" `pApply` EVar) pExpr1 `pAlt`
         pExpr2
  where f e1 op e2 = EAp (EAp op e1) e2
-}

pExpr1c :: Parser PartialExpr
pExpr1c = FoundOp <$> pLit "||" <*> pExpr1 `pAlt`
          pEmpty NoOp

{- |
>>> runParser pExpr2 $ clex 1 "x && y"
[(EAp (EAp (EVar "&&") (EVar "x")) (EVar "y"),[]),(EVar "x",[(1,"&&"),(1,"y")])]
-}
pExpr2 :: Parser CoreExpr
pExpr2 = assembleOp <$> pExpr3 <*> pExpr2c
{-
pExpr2 :: Parser CoreExpr
pExpr2 = pThen3 f pExpr3 (pLit "&&" `pApply` EVar) pExpr2 `pAlt`
         pExpr3
  where f e1 op e2 = EAp (EAp op e1) e2
-}

pExpr2c :: Parser PartialExpr
pExpr2c = FoundOp <$> pLit "&&" <*> pExpr2 `pAlt`
          pEmpty NoOp

{- |
>>> runParser pExpr3 $ clex 1 "x == y"
[(EAp (EAp (EVar "==") (EVar "x")) (EVar "y"),[]),(EVar "x",[(1,"=="),(1,"y")])]

>>> runParser pExpr3 $ clex 1 "x /= y"
[(EAp (EAp (EVar "/=") (EVar "x")) (EVar "y"),[]),(EVar "x",[(1,"/="),(1,"y")])]

>>> runParser pExpr3 $ clex 1 "x < y"
[(EAp (EAp (EVar "<") (EVar "x")) (EVar "y"),[]),(EVar "x",[(1,"<"),(1,"y")])]

>>> runParser pExpr3 $ clex 1 "x > y"
[(EAp (EAp (EVar ">") (EVar "x")) (EVar "y"),[]),(EVar "x",[(1,">"),(1,"y")])]

>>> runParser pExpr3 $ clex 1 "x <= y"
[(EAp (EAp (EVar "<=") (EVar "x")) (EVar "y"),[]),(EVar "x",[(1,"<="),(1,"y")])]

>>> runParser pExpr3 $ clex 1 "x >= y"
[(EAp (EAp (EVar ">=") (EVar "x")) (EVar "y"),[]),(EVar "x",[(1,">="),(1,"y")])]

>>> runParser pExpr3 $ clex 1 "x == y + z"
[(EAp (EAp (EVar "==") (EVar "x")) (EAp (EAp (EVar "+") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "==") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")]),(EVar "x",[(1,"=="),(1,"y"),(1,"+"),(1,"z")])]

>>> runParser pExpr3 $ clex 1 "x /= y + z"
[(EAp (EAp (EVar "/=") (EVar "x")) (EAp (EAp (EVar "+") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "/=") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")]),(EVar "x",[(1,"/="),(1,"y"),(1,"+"),(1,"z")])]

>>> runParser pExpr3 $ clex 1 "x < y + z"
[(EAp (EAp (EVar "<") (EVar "x")) (EAp (EAp (EVar "+") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "<") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")]),(EVar "x",[(1,"<"),(1,"y"),(1,"+"),(1,"z")])]

>>> runParser pExpr3 $ clex 1 "x > y + z"
[(EAp (EAp (EVar ">") (EVar "x")) (EAp (EAp (EVar "+") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar ">") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")]),(EVar "x",[(1,">"),(1,"y"),(1,"+"),(1,"z")])]

>>> runParser pExpr3 $ clex 1 "x <= y + z"
[(EAp (EAp (EVar "<=") (EVar "x")) (EAp (EAp (EVar "+") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "<=") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")]),(EVar "x",[(1,"<="),(1,"y"),(1,"+"),(1,"z")])]

>>> runParser pExpr3 $ clex 1 "x >= y + z"
[(EAp (EAp (EVar ">=") (EVar "x")) (EAp (EAp (EVar "+") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar ">=") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")]),(EVar "x",[(1,">="),(1,"y"),(1,"+"),(1,"z")])]
-}
pExpr3 :: Parser CoreExpr
pExpr3 = assembleOp <$> pExpr4 <*> pExpr3c

pExpr3c :: Parser PartialExpr
pExpr3c = FoundOp <$> pRelop <*> pExpr3 `pAlt`
          pEmpty NoOp

pRelop :: Parser String
pRelop = foldr1 pAlt $ map pLit [ "==", "/=", "<", "<=", ">", ">=" ]

{-
pExpr3 :: Parser CoreExpr
pExpr3 = pThen3 f pExpr4 (pRelop `pApply` EVar) pExpr4 `pAlt`
         pExpr4
  where f e1 op e2 = EAp (EAp op e1) e2
-}

{- |
>>> runParser pExpr4 $ clex 1 "x + y"
[(EAp (EAp (EVar "+") (EVar "x")) (EVar "y"),[]),(EVar "x",[(1,"+"),(1,"y")])]

>>> runParser pExpr4 $ clex 1 "x - y"
[(EAp (EAp (EVar "-") (EVar "x")) (EVar "y"),[]),(EVar "x",[(1,"-"),(1,"y")])]

>>> runParser pExpr4 $ clex 1 "x + y + z"
[(EAp (EAp (EVar "+") (EVar "x")) (EAp (EAp (EVar "+") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "+") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")]),(EVar "x",[(1,"+"),(1,"y"),(1,"+"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x + (y + z)"
[(EAp (EAp (EVar "+") (EVar "x")) (EAp (EAp (EVar "+") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"+"),(1,"("),(1,"y"),(1,"+"),(1,"z"),(1,")")])]

>>> runParser pExpr4 $ clex 1 "(x + y) + z"
[(EAp (EAp (EVar "+") (EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "+") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x - y - z"
[(EAp (EAp (EVar "-") (EVar "x")) (EVar "y"),[(1,"-"),(1,"z")]),(EVar "x",[(1,"-"),(1,"y"),(1,"-"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x - (y - z)"
[(EAp (EAp (EVar "-") (EVar "x")) (EAp (EAp (EVar "-") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"-"),(1,"("),(1,"y"),(1,"-"),(1,"z"),(1,")")])]

>>> runParser pExpr4 $ clex 1 "(x - y) - z"
[(EAp (EAp (EVar "-") (EAp (EAp (EVar "-") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "-") (EVar "x")) (EVar "y"),[(1,"-"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x + y - z"
[(EAp (EAp (EVar "+") (EVar "x")) (EAp (EAp (EVar "-") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "+") (EVar "x")) (EVar "y"),[(1,"-"),(1,"z")]),(EVar "x",[(1,"+"),(1,"y"),(1,"-"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x + (y - z)"
[(EAp (EAp (EVar "+") (EVar "x")) (EAp (EAp (EVar "-") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"+"),(1,"("),(1,"y"),(1,"-"),(1,"z"),(1,")")])]

>>> runParser pExpr4 $ clex 1 "(x + y) - z"
[(EAp (EAp (EVar "-") (EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "+") (EVar "x")) (EVar "y"),[(1,"-"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x - y + z"
[(EAp (EAp (EVar "-") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")]),(EVar "x",[(1,"-"),(1,"y"),(1,"+"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x - (y + z)"
[(EAp (EAp (EVar "-") (EVar "x")) (EAp (EAp (EVar "+") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"-"),(1,"("),(1,"y"),(1,"+"),(1,"z"),(1,")")])]

>>> runParser pExpr4 $ clex 1 "(x - y) + z"
[(EAp (EAp (EVar "+") (EAp (EAp (EVar "-") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "-") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x + y * z"
[(EAp (EAp (EVar "+") (EVar "x")) (EAp (EAp (EVar "*") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "+") (EVar "x")) (EVar "y"),[(1,"*"),(1,"z")]),(EVar "x",[(1,"+"),(1,"y"),(1,"*"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x + (y * z)"
[(EAp (EAp (EVar "+") (EVar "x")) (EAp (EAp (EVar "*") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"+"),(1,"("),(1,"y"),(1,"*"),(1,"z"),(1,")")])]

>>> runParser pExpr4 $ clex 1 "(x + y) * z"
[(EAp (EAp (EVar "*") (EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "+") (EVar "x")) (EVar "y"),[(1,"*"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x + y / z"
[(EAp (EAp (EVar "+") (EVar "x")) (EAp (EAp (EVar "/") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "+") (EVar "x")) (EVar "y"),[(1,"/"),(1,"z")]),(EVar "x",[(1,"+"),(1,"y"),(1,"/"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x + (y / z)"
[(EAp (EAp (EVar "+") (EVar "x")) (EAp (EAp (EVar "/") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"+"),(1,"("),(1,"y"),(1,"/"),(1,"z"),(1,")")])]

>>> runParser pExpr4 $ clex 1 "(x + y) / z"
[(EAp (EAp (EVar "/") (EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "+") (EVar "x")) (EVar "y"),[(1,"/"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x * y + z"
[(EAp (EAp (EVar "+") (EAp (EAp (EVar "*") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "*") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")]),(EVar "x",[(1,"*"),(1,"y"),(1,"+"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x * (y + z)"
[(EAp (EAp (EVar "*") (EVar "x")) (EAp (EAp (EVar "+") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"*"),(1,"("),(1,"y"),(1,"+"),(1,"z"),(1,")")])]

>>> runParser pExpr4 $ clex 1 "(x * y) + z"
[(EAp (EAp (EVar "+") (EAp (EAp (EVar "*") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "*") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x / y + z"
[(EAp (EAp (EVar "+") (EAp (EAp (EVar "/") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "/") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")]),(EVar "x",[(1,"/"),(1,"y"),(1,"+"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x / (y + z)"
[(EAp (EAp (EVar "/") (EVar "x")) (EAp (EAp (EVar "+") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"/"),(1,"("),(1,"y"),(1,"+"),(1,"z"),(1,")")])]

>>> runParser pExpr4 $ clex 1 "(x / y) + z"
[(EAp (EAp (EVar "+") (EAp (EAp (EVar "/") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "/") (EVar "x")) (EVar "y"),[(1,"+"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x - y * z"
[(EAp (EAp (EVar "-") (EVar "x")) (EAp (EAp (EVar "*") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "-") (EVar "x")) (EVar "y"),[(1,"*"),(1,"z")]),(EVar "x",[(1,"-"),(1,"y"),(1,"*"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x - (y * z)"
[(EAp (EAp (EVar "-") (EVar "x")) (EAp (EAp (EVar "*") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"-"),(1,"("),(1,"y"),(1,"*"),(1,"z"),(1,")")])]

>>> runParser pExpr4 $ clex 1 "(x - y) * z"
[(EAp (EAp (EVar "*") (EAp (EAp (EVar "-") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "-") (EVar "x")) (EVar "y"),[(1,"*"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x - y / z"
[(EAp (EAp (EVar "-") (EVar "x")) (EAp (EAp (EVar "/") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "-") (EVar "x")) (EVar "y"),[(1,"/"),(1,"z")]),(EVar "x",[(1,"-"),(1,"y"),(1,"/"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x - (y / z)"
[(EAp (EAp (EVar "-") (EVar "x")) (EAp (EAp (EVar "/") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"-"),(1,"("),(1,"y"),(1,"/"),(1,"z"),(1,")")])]

>>> runParser pExpr4 $ clex 1 "(x - y) / z"
[(EAp (EAp (EVar "/") (EAp (EAp (EVar "-") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "-") (EVar "x")) (EVar "y"),[(1,"/"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x * y - z"
[(EAp (EAp (EVar "-") (EAp (EAp (EVar "*") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "*") (EVar "x")) (EVar "y"),[(1,"-"),(1,"z")]),(EVar "x",[(1,"*"),(1,"y"),(1,"-"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x * (y - z)"
[(EAp (EAp (EVar "*") (EVar "x")) (EAp (EAp (EVar "-") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"*"),(1,"("),(1,"y"),(1,"-"),(1,"z"),(1,")")])]

>>> runParser pExpr4 $ clex 1 "(x * y) - z"
[(EAp (EAp (EVar "-") (EAp (EAp (EVar "*") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "*") (EVar "x")) (EVar "y"),[(1,"-"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x / y - z"
[(EAp (EAp (EVar "-") (EAp (EAp (EVar "/") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "/") (EVar "x")) (EVar "y"),[(1,"-"),(1,"z")]),(EVar "x",[(1,"/"),(1,"y"),(1,"-"),(1,"z")])]

>>> runParser pExpr4 $ clex 1 "x / (y - z)"
[(EAp (EAp (EVar "/") (EVar "x")) (EAp (EAp (EVar "-") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"/"),(1,"("),(1,"y"),(1,"-"),(1,"z"),(1,")")])]

>>> runParser pExpr4 $ clex 1 "(x / y) - z"
[(EAp (EAp (EVar "-") (EAp (EAp (EVar "/") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "/") (EVar "x")) (EVar "y"),[(1,"-"),(1,"z")])]
-}
pExpr4 :: Parser CoreExpr
pExpr4 = assembleOp <$> pExpr5 <*> pExpr4c

pExpr4c :: Parser PartialExpr
pExpr4c = FoundOp <$> pLit "+" <*> pExpr4 `pAlt`
          FoundOp <$> pLit "-" <*> pExpr5 `pAlt`
          pEmpty NoOp

{-
pExpr4 :: Parser CoreExpr
pExpr4 = pThen3 f pExpr5 (pLit "+" `pApply` EVar) pExpr4 `pAlt`
         pThen3 f pExpr5 (pLit "-" `pApply` EVar) pExpr5 `pAlt`
         pExpr5
  where f e1 op e2 = EAp (EAp op e1) e2
-}

{- |
>>> runParser pExpr5 $ clex 1 "x * y"
[(EAp (EAp (EVar "*") (EVar "x")) (EVar "y"),[]),(EVar "x",[(1,"*"),(1,"y")])]

>>> runParser pExpr5 $ clex 1 "x / y"
[(EAp (EAp (EVar "/") (EVar "x")) (EVar "y"),[]),(EVar "x",[(1,"/"),(1,"y")])]

>>> runParser pExpr5 $ clex 1 "x * y * z"
[(EAp (EAp (EVar "*") (EVar "x")) (EAp (EAp (EVar "*") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "*") (EVar "x")) (EVar "y"),[(1,"*"),(1,"z")]),(EVar "x",[(1,"*"),(1,"y"),(1,"*"),(1,"z")])]

>>> runParser pExpr5 $ clex 1 "x * (y * z)"
[(EAp (EAp (EVar "*") (EVar "x")) (EAp (EAp (EVar "*") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"*"),(1,"("),(1,"y"),(1,"*"),(1,"z"),(1,")")])]

>>> runParser pExpr5 $ clex 1 "(x * y) * z"
[(EAp (EAp (EVar "*") (EAp (EAp (EVar "*") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "*") (EVar "x")) (EVar "y"),[(1,"*"),(1,"z")])]

>>> runParser pExpr5 $ clex 1 "x / y / z"
[(EAp (EAp (EVar "/") (EVar "x")) (EVar "y"),[(1,"/"),(1,"z")]),(EVar "x",[(1,"/"),(1,"y"),(1,"/"),(1,"z")])]

>>> runParser pExpr5 $ clex 1 "x / (y / z)"
[(EAp (EAp (EVar "/") (EVar "x")) (EAp (EAp (EVar "/") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"/"),(1,"("),(1,"y"),(1,"/"),(1,"z"),(1,")")])]

>>> runParser pExpr5 $ clex 1 "(x / y) / z"
[(EAp (EAp (EVar "/") (EAp (EAp (EVar "/") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "/") (EVar "x")) (EVar "y"),[(1,"/"),(1,"z")])]

>>> runParser pExpr5 $ clex 1 "x * y / z"
[(EAp (EAp (EVar "*") (EVar "x")) (EAp (EAp (EVar "/") (EVar "y")) (EVar "z")),[]),(EAp (EAp (EVar "*") (EVar "x")) (EVar "y"),[(1,"/"),(1,"z")]),(EVar "x",[(1,"*"),(1,"y"),(1,"/"),(1,"z")])]

>>> runParser pExpr5 $ clex 1 "x * (y / z)"
[(EAp (EAp (EVar "*") (EVar "x")) (EAp (EAp (EVar "/") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"*"),(1,"("),(1,"y"),(1,"/"),(1,"z"),(1,")")])]

>>> runParser pExpr5 $ clex 1 "(x * y) / z"
[(EAp (EAp (EVar "/") (EAp (EAp (EVar "*") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "*") (EVar "x")) (EVar "y"),[(1,"/"),(1,"z")])]

>>> runParser pExpr5 $ clex 1 "x / y * z"
[(EAp (EAp (EVar "/") (EVar "x")) (EVar "y"),[(1,"*"),(1,"z")]),(EVar "x",[(1,"/"),(1,"y"),(1,"*"),(1,"z")])]

>>> runParser pExpr5 $ clex 1 "x / (y * z)"
[(EAp (EAp (EVar "/") (EVar "x")) (EAp (EAp (EVar "*") (EVar "y")) (EVar "z")),[]),(EVar "x",[(1,"/"),(1,"("),(1,"y"),(1,"*"),(1,"z"),(1,")")])]

>>> runParser pExpr5 $ clex 1 "(x / y) * z"
[(EAp (EAp (EVar "*") (EAp (EAp (EVar "/") (EVar "x")) (EVar "y"))) (EVar "z"),[]),(EAp (EAp (EVar "/") (EVar "x")) (EVar "y"),[(1,"*"),(1,"z")])]
-}
pExpr5 :: Parser CoreExpr
pExpr5 = assembleOp <$> pExpr6 <*> pExpr5c

pExpr5c :: Parser PartialExpr
pExpr5c = FoundOp <$> pLit "*" <*> pExpr5 `pAlt`
          FoundOp <$> pLit "/" <*> pExpr6 `pAlt`
          pEmpty NoOp

{-
pExpr5 :: Parser CoreExpr
pExpr5 = pThen3 f pExpr6 (pLit "*" `pApply` EVar) pExpr5 `pAlt`
         pThen3 f pExpr6 (pLit "/" `pApply` EVar) pExpr6 `pAlt`
         pExpr6
  where f e1 op e2 = EAp (EAp op e1) e2
-}

{- |
>>> runParser pExpr6 $ clex 1 "42"
[(ENum 42,[])]

>>> runParser pExpr6 $ clex 1 "x"
[(EVar "x",[])]

>>> runParser pExpr6 $ clex 1 "f x"
[(EAp (EVar "f") (EVar "x"),[]),(EVar "f",[(1,"x")])]

>>> runParser pExpr6 $ clex 1 "f x y"
[(EAp (EAp (EVar "f") (EVar "x")) (EVar "y"),[]),(EAp (EVar "f") (EVar "x"),[(1,"y")]),(EVar "f",[(1,"x"),(1,"y")])]
-}
pExpr6 :: Parser CoreExpr
pExpr6 = mkApChain <$> pOneOrMore pAexpr

mkApChain :: [CoreExpr] -> CoreExpr
mkApChain = foldl1 EAp

parse :: String -> CoreProgram
parse src = syntax src $ clex 1 src


----------------------------------------------------------------------------------------
-- prelude
----------------------------------------------------------------------------------------
{- |
>>> putStrLn . pprint . parse $ preludeCode
I x = x ;
K x y = x ;
K1 x y = y ;
S f g x = f x (g x) ;
compose f g x = f (g x) ;
twice f = compose f f
-}
preludeCode :: String
preludeCode
  = unlines [ "I x = x ;"
            , "K x y = x ;"
            , "K1 x y = y ;"
            , "S f g x = f x (g x) ;"
            , "compose f g x = f (g x) ;"
            , "twice f = compose f f"
            ]

{- |
>>> putStrLn . pprint $ preludeDefs
I x = x ;
K x y = x ;
K1 x y = y ;
S f g x = f x (g x) ;
compose f g x = f (g x) ;
twice f = compose f f
-}
preludeDefs :: CoreProgram
preludeDefs = parse preludeCode
{-
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
-}

----------------------------------------------------------------------------------------
-- sample code and prelude
----------------------------------------------------------------------------------------
{- |
>>> putStrLn . pprint . parse $ testProgram
f = 3 ;
g x y = let
          z = y
        in z ;
h x = case let
             y = x
           in y of
        <1> -> 2 ;
        <2> -> 5
-}
testProgram :: String
testProgram = unlines [ "f = 3 ;"
                      , "g x y = let z = y in z ;"
                      , "h x = case (let y = x in y) of"
                      , "        <1> -> 2;"
                      , "        <2> -> 5"
                      ]

-- これは区別ができない
{- |
>>> putStrLn . pprint . parse $ hangEleProblem
f x y = case x of
          <1> -> case y of
                   <1> -> 1 ;
                   <2> -> 2
-}
hangEleProblem :: String
hangEleProblem = unlines [ "f x y = case x of"
                         , "          <1> -> case y of"
                         , "                   <1> -> 1;"
                         , "          <2> -> 2"
                         ]
{- |
f x y = case x of
         <1> -> case y of
                  <1> -> 1 ;
         <2> -> 2
-}
hangEleProblem' :: String
hangEleProblem' = unlines [ "f x y = case x of"
                         , "          <1> -> (case y of"
                         , "                    <1> -> 1);"
                         , "          <2> -> 2"
                         ]

{- |
>>> putStrLn . pprint $ sampleProgram
main = double 42 ;
double x = x + x ;
f x = letrec
        y = x + 1;
        z = y + 2
      in z ;
g = x + y > p * length xs
-}
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
