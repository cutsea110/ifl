module Lambda.Mark2 where

import Data.List (mapAccumL)
import qualified Data.Set as Set
import Data.Set (Set)

import Language
import Utils
import Iseq

import Prelude hiding (head)

head :: [a] -> a
head [] = error "head: empty list"
head (x:_) = x

type AnnExpr a b = (b, AnnExpr' a b)

data AnnExpr' a b = AVar Name
                  | ANum Int
                  | AConstr Int Int
                  | AAp (AnnExpr a b) (AnnExpr a b)
                  | ALet Bool [AnnDefn a b] (AnnExpr a b)
                  | ACase (AnnExpr a b) [AnnAlt a b]
                  | ALam [a] (AnnExpr a b)
                  deriving (Show, Eq)

type AnnDefn a b = (a, AnnExpr a b)
type AnnAlt a b = (Int, [a], AnnExpr a b)
type AnnProgram a b = [(Name, [a], AnnExpr a b)]

{- |
>>> putStrLn . pprint . lambdaLift $ parse "f = \\x -> x + 1"
f x_1 = x_1 + 1
>>> putStrLn . pprint . lambdaLift $ parse "f = \\x -> x + y"
f x_1 = x_1 + y
>>> putStrLn . pprint . lambdaLift $ parse "f = \\x y -> x + y"
f x_1 y_2 = x_1 + y_2
>>> putStrLn . pprint . lambdaLift $ parse "f = \\x y -> x + 3"
f x_1 y_2 = x_1 + 3
>>> putStrLn . pprint . lambdaLift $ parse "f x = let g = x + 1 in g"
f x_0 = let
          g_1 = x_0 + 1
        in g_1
>>> putStrLn . pprint . lambdaLift $ parse "f x = let g = \\y -> x + y in g 1"
f x_0 = let
          g_1 = sc_2 x_0
        in g_1 1 ;
sc_2 x_3 y_4 = x_3 + y_4
>>> putStrLn . pprint . lambdaLift $ parse "f x = let g = (\\y -> y + 1) in g (g x)"
f x_0 = g_1 (g_1 x_0) ;
g_1 y_3 = y_3 + 1
-}
lambdaLift :: CoreProgram -> CoreProgram
lambdaLift = collectSCs . rename . abstract . freeVars

abstract :: AnnProgram Name (Set Name) -> CoreProgram
abstract prog = [ (sc_name, args, abstract_e rhs)
                | (sc_name, args, rhs) <- prog
                ]

{- |
>>> abstract_e (Set.fromList ["x"], AVar "x")
EVar "x"

>>> abstract_e (Set.fromList [], AVar "x")
EVar "x"

>>> abstract_e (Set.fromList ["x"], AVar "x")
EVar "x"

>>> abstract_e (Set.fromList ["f","x"], AAp (Set.fromList ["f"], AVar "f") (Set.fromList ["x"], AVar "x"))
EAp (EVar "f") (EVar "x")

>>> abstract_e (Set.fromList [], AAp (Set.fromList [], AVar "f") (Set.fromList [], AVar "x"))
EAp (EVar "f") (EVar "x")

>>> abstract_e (Set.fromList ["x","y"], ALet False [("x",(Set.fromList [],ANum 1)),("y",(Set.fromList [],ANum 2))] (Set.fromList ["x"],AVar "x"))
ELet False [("x",ENum 1),("y",ENum 2)] (EVar "x")

>>> abstract_e (Set.fromList ["f"], ALet True [("g",(Set.fromList ["f","g"],AAp (Set.fromList ["f"],AVar "f") (Set.fromList ["g"],AVar "g")))] (Set.fromList ["g"],AVar "g"))
ELet True [("g",EAp (EVar "f") (EVar "g"))] (EVar "g")

>>> abstract_e (Set.fromList ["f","y"], ALam ["x"] (Set.fromList ["f","x","y"],AAp (Set.fromList ["f"],AVar "f") (Set.fromList ["x"],AVar "x")))
EAp (EAp (ELet False [("sc",ELam ["f","y","x"] (EAp (EVar "f") (EVar "x")))] (EVar "sc")) (EVar "f")) (EVar "y")
-}
abstract_e :: AnnExpr Name (Set Name) -> CoreExpr
abstract_e (free, AVar v) = EVar v
abstract_e (free, ANum n) = ENum n
abstract_e (free, AAp e1 e2) = EAp (abstract_e e1) (abstract_e e2)
abstract_e (free, ALet is_rec defns body)
  = ELet is_rec [ (name, abstract_e body)
                | (name, body) <- defns
                ] (abstract_e body)
abstract_e (free, ALam args body)
  = foldl EAp sc (map EVar fvList)
  where fvList = Set.toList free
        sc     = ELet nonRecursive [("sc", sc_rhs)] (EVar "sc")
        sc_rhs = ELam (fvList ++ args) (abstract_e body)
abstract_e (free, AConstr t a)  = EConstr t a
abstract_e (free, ACase e alts) = abstract_case free e alts

abstract_case :: Set Name
              -> AnnExpr Name (Set Name)
              -> [AnnAlt Name (Set Name)]
              -> CoreExpr
abstract_case free e alts = ECase (abstract_e e) alts'
  where alts' = [ (tag, args, abstract_e rhs)
                | (tag, args, rhs) <- alts
                ]

{- |
>>> rename $ parse "g x = letrec f = \\x -> x + x in f 3"
[("g",["x_0"],ELet True [("f_1",ELam ["x_2"] (EAp (EAp (EVar "+") (EVar "x_2")) (EVar "x_2")))] (EAp (EVar "f_1") (ENum 3)))]
-}
rename :: CoreProgram -> CoreProgram
rename prog
  = snd (mapAccumL rename_sc initialNameSupply prog)
  where
    rename_sc ns (sc_name, args, rhs)
      = (ns2, (sc_name, args', rhs'))
      where (ns1, args', env) = newNames ns args
            (ns2, rhs') = rename_e env ns1 rhs

type NameSupply = Int
initialNameSupply :: NameSupply
initialNameSupply = 0
getName :: NameSupply -> Name -> (NameSupply, Name)
getName name_supply prefix = (name_supply + 1, makeName prefix name_supply)
getNames :: NameSupply -> [Name] -> (NameSupply, [Name])
getNames name_supply prefixes
  = (name_supply + length prefixes, zipWith makeName prefixes [name_supply ..])
makeName prefix ns = prefix ++ "_" ++ show ns

newNames :: NameSupply -> [Name] -> (NameSupply, [Name], Assoc Name Name)
newNames ns old_names = (ns', new_names, env)
  where (ns', new_names) = getNames ns old_names
        env = zip old_names new_names

rename_e :: Assoc Name Name
         -> NameSupply
         -> CoreExpr
         -> (NameSupply, CoreExpr)
rename_e env ns (EVar v)    = (ns, EVar (aLookup env v v))
rename_e env ns (ENum n)    = (ns, ENum n)
rename_e env ns (EAp e1 e2) = (ns2, EAp e1' e2')
    where (ns1, e1') = rename_e env ns  e1
          (ns2, e2') = rename_e env ns1 e2
rename_e env ns (ELam args body)
  = (ns2, ELam args' body') -- NOTE: The book use ns1 here.
  where (ns1, args', env') = newNames ns args
        (ns2, body') = rename_e (env' ++ env) ns1 body
rename_e env ns (ELet is_rec defns body)
  = (ns3, ELet is_rec (zip binders' rhss') body')
  where binders = bindersOf defns
        -- NOTE: The book's name supply usage is different here.
        (ns1, binders', env') = newNames ns binders
        body_env = env' ++ env
        (ns2, rhss') = mapAccumL (rename_e rhsEnv) ns1 (rhssOf defns)
        rhsEnv | is_rec   = body_env
               | otherwise = env
        (ns3, body') = rename_e body_env ns2 body
rename_e env ns (EConstr t a) = (ns, EConstr t a)
rename_e env ns (ECase e alts) = rename_case env ns e alts

rename_case :: Assoc Name Name
            -> NameSupply
            -> CoreExpr
            -> [CoreAlt]
            -> (NameSupply, CoreExpr)
rename_case env ns e alts
  = (ns2, ECase e' alts')
  where (ns1, e') = rename_e env ns e
        (ns2, alts') = mapAccumL (rename_alt env) ns alts

rename_alt :: Assoc Name Name
           -> NameSupply
           -> CoreAlt
           -> (NameSupply, CoreAlt)
rename_alt env ns (tag, args, rhs)
  = (ns2, (tag, args', rhs'))
  where (ns1, args', env') = newNames ns args
        (ns2, rhs') = rename_e (env' ++ env) ns1 rhs

collectSCs :: CoreProgram -> CoreProgram
collectSCs prog
  = concatMap collect_one_sc prog
  where
    -- exercise 6.5
    -- If the rhs is a variable referring to an existing supercombinator,
    -- we can just reuse that supercombinator instead of creating a new one.
    collect_one_sc (sc_name, args, ELet False [(name, ELam args' body')] (EVar new_name))
      | new_name == name = (sc_name, args ++ args', body'') : scs
      where (scs, body'') = collectSCs_e body'
    collect_one_sc (sc_name, args, rhs) = (sc_name, args, rhs') : scs
      where (scs, rhs') = collectSCs_e rhs

collectSCs_e :: CoreExpr -> ([CoreScDefn], CoreExpr)
collectSCs_e (EVar v) = ([], EVar v)
collectSCs_e (ENum n) = ([], ENum n)
collectSCs_e (EAp e1 e2)
  = (scs1 ++ scs2, EAp e1' e2')
    where (scs1, e1') = collectSCs_e e1
          (scs2, e2') = collectSCs_e e2
collectSCs_e (ELam args body)
  = (scs, ELam args body')
  where (scs, body') = collectSCs_e body
collectSCs_e (EConstr t a) = ([], EConstr t a)
collectSCs_e (ECase e alts)
  = (scs_e ++ scs_alts, ECase e' alts')
  where (scs_e, e') = collectSCs_e e
        (scs_alts, alts') = mapAccumL collectSCs_alt [] alts
        collectSCs_alt scs (tag, args, rhs)
          = (scs++scs_rhs, (tag, args, rhs'))
          where (scs_rhs, rhs') = collectSCs_e rhs
collectSCs_e (ELet is_rec defns body)
  = (rhss_scs++body_scs++local_scs, mkELet is_rec non_scs' body')
  where (rhss_scs, defns') = mapAccumL collectSCs_d [] defns

        scs'     = [(name, rhs) | (name, rhs) <- defns', isELam rhs]
        non_scs' = [(name, rhs) | (name, rhs) <- defns', not (isELam rhs)]
        local_scs = [(name, args, body) | (name, ELam args body) <- scs']

        (body_scs, body') = collectSCs_e body

        -- exercise 6.6
        -- this pattern means like this: g = let sc = \args -> body in sc.
        collectSCs_d scs (name, ELet False [(name', ELam args' body')] (EVar name''))
          | name' == name'' = case collectSCs_e body' of
              (scs'', body'') -> (scs++scs'', (name, ELam args' body''))
        collectSCs_d scs (name, rhs) = (scs++rhs_scs, (name, rhs'))
          where (rhs_scs, rhs') = collectSCs_e rhs

isELam :: Expr a -> Bool
isELam (ELam _ _) = True
isELam _          = False

mkELet :: IsRec -> [(a, Expr a)] -> Expr a -> Expr a
mkELet _      []    body = body
mkELet is_rec defns body = ELet is_rec defns body

runS :: String -> String
runS = pprint . lambdaLift . parse

{- |
>>> let expr = (EVar "x")
>>> freeVars_e Set.empty expr
(fromList [],AVar "x")

>>> let expr = (EVar "x")
>>> freeVars_e (Set.fromList ["x", "y"]) expr
(fromList ["x"],AVar "x")

>>> let expr = (EAp (EVar "f") (EVar "x"))
>>> freeVars_e (Set.fromList ["f", "x", "y"]) expr
(fromList ["f","x"],AAp (fromList ["f"],AVar "f") (fromList ["x"],AVar "x"))

>>> let expr = (EAp (EVar "f") (EVar "x"))
>>> freeVars_e (Set.fromList ["y"]) expr
(fromList [],AAp (fromList [],AVar "f") (fromList [],AVar "x"))

>>> let expr = (ELam ["x"] (EAp (EVar "f") (EVar "x")))
>>> freeVars_e (Set.fromList ["x", "y"]) expr
(fromList [],ALam ["x"] (fromList ["x"],AAp (fromList [],AVar "f") (fromList ["x"],AVar "x")))


>>> let expr = (ELet False [("x", ENum 1), ("y", ENum 2)] (EVar "x"))
>>> freeVars_e (Set.fromList ["x", "y", "z"]) expr
(fromList [],ALet False [("x",(fromList [],ANum 1)),("y",(fromList [],ANum 2))] (fromList ["x"],AVar "x"))

>>> let expr = (ELet True [("x", EVar "y"), ("y", ENum 2)] (EVar "x"))
>>> freeVars_e (Set.fromList ["x", "y", "z"]) expr
(fromList [],ALet True [("x",(fromList ["y"],AVar "y")),("y",(fromList [],ANum 2))] (fromList ["x"],AVar "x"))

>>> let expr = (ELet True [("g", EAp (EVar "f") (EVar "g"))] (EVar "g"))
>>> freeVars_e (Set.fromList ["fix", "f"]) expr
(fromList ["f"],ALet True [("g",(fromList ["f","g"],AAp (fromList ["f"],AVar "f") (fromList ["g"],AVar "g")))] (fromList ["g"],AVar "g"))

-}
freeVars_e :: Set Name                    -- ^ Candidates for free variables
           -> CoreExpr                    -- ^ Expression to annotate
           -> AnnExpr Name (Set Name)     -- ^ Annotated result
freeVars_e lv (ENum n) = (Set.empty, ANum n)
freeVars_e lv (EVar v)
  | v `Set.member` lv = (Set.singleton v, AVar v)
  | otherwise         = (Set.empty,      AVar v)
freeVars_e lv (EAp e1 e2)
  = (Set.union (freeVarsOf e1') (freeVarsOf e2'), AAp e1' e2')
    where e1' = freeVars_e lv e1
          e2' = freeVars_e lv e2
freeVars_e lv (ELam args body)
  = (Set.difference (freeVarsOf body') (Set.fromList args), ALam args body')
    where body'  = freeVars_e new_lv body
          new_lv = Set.union lv (Set.fromList args)
freeVars_e lv (ELet is_rec defns body)
  = (Set.union defnsFree bodyFree, ALet is_rec defns' body')
  where binders               = bindersOf defns
        binderSet             = Set.fromList binders
        body_lv               = Set.union lv binderSet
        rhs_lv | is_rec       = body_lv
               | otherwise    = lv
        rhss'                 = map (freeVars_e rhs_lv) (rhssOf defns)
        defns'                = zip binders rhss'
        freeInValues          = Set.unions (map freeVarsOf rhss')
        defnsFree | is_rec    = Set.difference freeInValues binderSet
                  | otherwise = freeInValues
        body'                 = freeVars_e body_lv body
        bodyFree              = Set.difference (freeVarsOf body') binderSet
freeVars_e lv (ECase e alts)  = freeVars_case lv e alts
freeVars_e lv (EConstr t a)   = (Set.empty, AConstr t a)

freeVars_case :: Set Name -> CoreExpr -> [CoreAlt] -> AnnExpr Name (Set Name)
freeVars_case lv e alts = (Set.union (freeVarsOf e') free, ACase e' alts')
  where e' = freeVars_e lv e
        alts' = [ (tag, args, freeVars_e (Set.union lv (Set.fromList args)) rhs)
                | (tag, args, rhs) <- alts
                ]
        free  = Set.unions $ map freeVarsOf_alt alts'

freeVars :: CoreProgram -> AnnProgram Name (Set Name)
freeVars prog = [ (name, args, freeVars_e (Set.fromList args) body)
                |(name, args, body) <- prog
                ]

freeVarsOf :: AnnExpr a (Set Name) -> Set Name
freeVarsOf (free_vars, expr) = free_vars

freeVarsOf_alt :: AnnAlt Name (Set Name) -> Set Name
freeVarsOf_alt (tag, args, rhs)
  = Set.difference (freeVarsOf rhs) (Set.fromList args)


pprintAnn :: (a -> IseqRep)               -- ^ Pretty-print annotation on variables
          -> (b -> IseqRep)               -- ^ Pretty-print annotation on expressions
          -> AnnProgram a b               -- ^ Program to pretty-print
          -> IseqRep                      -- ^ Resulting document
pprintAnn ppra pprb prog = iInterleave iNewline (map (pprintAnnSc ppra pprb) prog)

pprintAnnSc :: (a -> IseqRep)             -- ^ Pretty-print annotation on variables
            -> (b -> IseqRep)             -- ^ Pretty-print annotation on expressions
            -> (Name, [a], AnnExpr a b)   -- ^ Supercombinator to pretty-print
            -> IseqRep                    -- ^ Resulting document
pprintAnnSc ppra pprb (name, args, body) =
  iConcat [ iStr name, iStr " ", iInterleave iSpace (map ppra args)
          , iStr " = ", pprintAnnExpr ppra pprb 0 body
          ]

{- |
>>> let p expr = putStr $ iDisplay $ pprintAnnExpr (iStr . id) (iStr . show) 0 expr
>>> p ((), AVar "x")
{- () -} x

>>> p ((), ALam ["y"] ((), AVar "y"))
{- () -} \y -> ({- () -} y)

>>> p ((), ACase ((), AVar "x") [(1, ["y"], ((), AVar "y")), (2, ["z"], ((), ANum 3))])
{- () -} case ({- () -} x) of
  <1> y -> ({- () -} y);
  <2> z -> ({- () -} 3)

>>> p ((), ALet False [("x", ((), ANum 1)), ("y", ((), ANum 2))] ((), AVar "x"))
{- () -} let
  x = ({- () -} 1);
  y = ({- () -} 2)
in
  ({- () -} x)

>>> p ((), ALet True [("x", ((), ANum 1)), ("y", ((), ANum 2))] ((), AVar "x"))
{- () -} letrec
  x = ({- () -} 1);
  y = ({- () -} 2)
in
  ({- () -} x)

>>> p ((), AAp ((), AAp ((), AVar "f") ((), AVar "x")) ((), AVar "y"))
{- () -} ({- () -} (({- () -} f) ({- () -} x))) {- () -} y

-- 3 + 6
>>> p ((), AAp ((), AAp ((), AVar "+") ((), ANum 3)) ((), ANum 6))
{- () -} ({- () -} (({- () -} +) ({- () -} 3))) {- () -} 6

-}
pprintAnnExpr :: (a -> IseqRep)           -- ^ Pretty-print annotation on variables
              -> (b -> IseqRep)           -- ^ Pretty-print annotation on expressions
              -> Int                      -- ^ Precedence level
              -> AnnExpr a b              -- ^ Expression to pretty-print
              -> IseqRep                  -- ^ Resulting document
pprintAnnExpr ppra pprb d (ann, expr)
  = let doc = iConcat [ iStr "{- ", pprb ann, iStr " -} "
                      , pprintAnnExpr' ppra pprb d expr
                      ]
    in if d > 0 then iParen doc else doc


pprintAnnExpr' :: (a -> IseqRep)          -- ^ Pretty-print annotation on variables
               -> (b -> IseqRep)          -- ^ Pretty-print annotation on expressions
               -> Int                     -- ^ Precedence level
               -> AnnExpr' a b            -- ^ Expression to pretty-print
               -> IseqRep                 -- ^ Resulting document
pprintAnnExpr' _ _ _ (AVar name) = iStr name
pprintAnnExpr' _ _ _ (ANum n) = iNum n
pprintAnnExpr' _ _ _ (AConstr tag arity)
  = iConcat [ iStr "Pack{", iNum tag, iStr ",", iNum arity, iStr "}" ]
pprintAnnExpr' ppra pprb d (AAp e1 e2)
  = let e1Doc = case e1 of
          (_, AAp _ _) -> pprintAnnExpr ppra pprb (d + 1) e1
          _            -> pprintAnnExpr ppra pprb d       e1
        e2Doc = case e2 of
          (_, AAp _ _) -> pprintAnnExpr ppra pprb (d + 1) e2
          _            -> pprintAnnExpr ppra pprb d       e2
        doc = iConcat [e1Doc, iStr " ", e2Doc]
    in if d > 0 then iParen doc else doc
pprintAnnExpr' ppra pprb d (ALet isRec defs body)
  = let d' = d + 1
        letDoc = if isRec then iStr "letrec" else iStr "let"
        defsDoc = iInterleave (iConcat [iStr ";", iNewline])
                  (map (pprintAnnDef ppra pprb d') defs)
        bodyDoc = pprintAnnExpr ppra pprb d' body
        doc     = iConcat [ letDoc, iNewline
                          , defsDoc, iNewline
                          , iStr "in", iNewline
                          , iStr (space (2 * d')), bodyDoc
                          ]
    in if d > 0 then iParen doc else doc
pprintAnnExpr' ppra pprb d (ACase expr alts)
  = let exprDoc = pprintAnnExpr ppra pprb (d + 1) expr
        altsDoc = iInterleave (iConcat [iStr ";", iNewline])
                  (map (pprintAnnAlt ppra pprb (d + 1)) alts)
        doc     = iConcat [ iStr "case ", exprDoc, iStr " of", iNewline
                          , altsDoc
                          ]
    in if d > 0 then iParen doc else doc
pprintAnnExpr' ppra pprb d (ALam args body)
  = let argsDoc = iInterleave iSpace (map ppra args)
        bodyDoc = pprintAnnExpr ppra pprb (d + 1) body
        doc     = iConcat [ iStr "\\", argsDoc, iStr " -> ", bodyDoc ]
    in if d > 0 then iParen doc else doc

pprintAnnDef :: (a -> IseqRep) -> (b -> IseqRep) -> Int -> AnnDefn a b -> IseqRep
pprintAnnDef ppra pprb d (name, expr)
  = let exprDoc = pprintAnnExpr ppra pprb d expr
    in iConcat [ iStr (space (d * 2)), ppra name, iStr " = ", exprDoc ]

pprintAnnAlt :: (a -> IseqRep) -> (b -> IseqRep) -> Int -> AnnAlt a b -> IseqRep
pprintAnnAlt ppra pprb d (tag, args, expr)
  = let argsDoc = iInterleave iSpace (map ppra args)
        exprDoc = pprintAnnExpr ppra pprb d expr
    in iConcat [ iStr (space (d * 2)), iStr "<", iNum tag, iStr ">", sep, argsDoc
               , iStr " -> ", exprDoc
               ]
       where sep = if null args then iNil else iSpace
