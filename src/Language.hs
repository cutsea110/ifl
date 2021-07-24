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

sampleProgram :: CoreProgram
sampleProgram
  = [ ("main", [], EAp (EVar "double") (ENum 21))
    , ("double", ["x"], EAp (EAp (EVar "+") (EVar "x")) (EVar "x"))
    , ("f", ["x"]
      , ELet recursive
        [("y", EAp (EAp (EVar "+") (EVar "x")) (ENum 1))
        ,("z", EAp (EAp (EVar "+") (EVar "y")) (ENum 1))
        ]
        (EVar "z"))
    ]

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



-- pretty printer
pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)

{--
pprExpr :: CoreExpr -> String
pprExpr (ENum n)    = show n
pprExpr (EVar v)    = v
pprExpr (EAp e1 e2) = pprExpr e1 ++ " " ++ pprExpr e2

pprAExpr :: CoreExpr -> String
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise      = "(" ++ pprExpr e ++ ")"

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 $ replicate n e2
--}

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

pprProgram :: CoreProgram -> Iseqrep
pprProgram scdefns = iInterleave iNL $ map pprScDefn scdefns

pprScDefn :: CoreScDefn -> Iseqrep
pprScDefn (name, args, expr)
  = iConcat [ iStr name, iSpace, pprArgs args
            , iStr " = "
            , iIndent (pprExpr expr)
            ]

iSpace :: Iseq iseq => iseq
iSpace = iStr " "

pprArgs :: [String] -> Iseqrep
pprArgs args = iInterleave iSpace $ map iStr args

pprExpr :: CoreExpr -> Iseqrep
pprExpr (ENum n) = iStr (show n)
pprExpr (EVar v) = iStr v
pprExpr (EConstr tag arity)
  = iConcat [ iStr "Pack{"
            , iStr (show tag), iStr ",", iStr (show arity)
            , iStr "}"
            ]
pprExpr (EAp e1 e2)
  = iConcat [ pprExpr e1, iStr " ", pprAExpr e2 ]
pprExpr (ELet isrec defns expr)
  = iConcat [ iStr keyword, iNewline
            , iStr "  ", iIndent (pprDefns defns), iNewline
            , iStr "in ", pprExpr expr
            ]
  where
    keyword | not isrec = "let"
            | isrec     = "letrec"
pprExpr (ECase e alts)
  = iConcat [ iStr "case ", iIndent (pprExpr e), iStr " of", iNewline
            , iStr "  ", iIndent (iInterleave iNL (map pprAlt alts))
            ]

pprExpr (ELam args e)
  = iConcat [ iStr "\\ ", pprArgs args, iStr " -> "
            , iIndent (pprExpr e)
            ]

iNL :: Iseq iseq => iseq
iNL = iStr " ;" `iAppend` iNewline

pprAlt :: CoreAlter -> Iseqrep
pprAlt (i, args, expr)
  = iConcat [ iStr "<", iStr (show i), iStr ">", iSpace, pprArgs args
            , iStr " -> ", iIndent (pprExpr expr)
            ]

pprAExpr :: CoreExpr -> Iseqrep
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise      = iStr "(" `iAppend` pprExpr e `iAppend` iStr ")"

pprDefns :: [(Name, CoreExpr)] -> Iseqrep
pprDefns defns = iInterleave sep (map pprDefn defns)
  where
    sep = iConcat [ iStr ";", iNewline ]

pprDefn :: (Name, CoreExpr) -> Iseqrep
pprDefn (name, expr)
  = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]

iConcat :: Iseq iseq => [iseq] -> iseq
iConcat = foldr iAppend iNil

iInterleave :: Iseq iseq => iseq -> [iseq] -> iseq
iInterleave sep [] = iNil
iInterleave sep [x] = x
iInterleave sep (x:xs)
  = x `iAppend` sep `iAppend` iInterleave sep xs
