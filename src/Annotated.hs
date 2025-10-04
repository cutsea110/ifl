module Annotated where

import qualified Data.Set as Set
import Data.Set (Set)

import Language
import Utils
import Iseq

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
>>> import qualified Data.Set as Set
>>> import Data.Set (Set)

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
freeVars_e lv (EConstr t a)   = error "freeVars_e: no case for constructors"

freeVars_case :: Set Name -> CoreExpr -> [CoreAlt] -> AnnExpr Name (Set Name)
freeVars_case lv e alts = error "freeVars_case: not yet written"

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
>>> let expr = ((), AVar "x")
>>> putStr $ iDisplay $ pprintAnnExpr (iStr . id) (iStr . show) 0 expr
{- () -} x

>>> let expr = ((), ALam ["y"] ((), AVar "y"))
>>> putStr $ iDisplay $ pprintAnnExpr (iStr . id) (iStr . show) 0 expr
{- () -} \y -> ({- () -} y)

>>> let expr = ((), ACase ((), AVar "x") [(1, ["y"], ((), AVar "y")), (2, ["z"], ((), ANum 3))])
>>> putStr $ iDisplay $ pprintAnnExpr (iStr . id) (iStr . show) 0 expr
{- () -} case ({- () -} x) of
  <1> y -> ({- () -} y)
  <2> z -> ({- () -} 3)

>>> let expr = ((), ALet False [("x", ((), ANum 1)), ("y", ((), ANum 2))] ((), AVar "x"))
>>> putStr $ iDisplay $ pprintAnnExpr (iStr . id) (iStr . show) 0 expr
{- () -} let
  x = ({- () -} 1);
  y = ({- () -} 2)
in
  ({- () -} x)

>>> let expr = ((), ALet True [("x", ((), ANum 1)), ("y", ((), ANum 2))] ((), AVar "x"))
>>> putStr $ iDisplay $ pprintAnnExpr (iStr . id) (iStr . show) 0 expr
{- () -} letrec
  x = ({- () -} 1);
  y = ({- () -} 2)
in
  ({- () -} x)

>>> let expr = ((), AAp ((), AAp ((), AVar "f") ((), AVar "x")) ((), AVar "y"))
>>> putStr $ iDisplay $ pprintAnnExpr (iStr . id) (iStr . show) 0 expr
{- () -} ({- () -} (({- () -} f) ({- () -} x))) {- () -} y

-- 3 + 6
>>> let expr = ((), AAp ((), AAp ((), AVar "+") ((), ANum 3)) ((), ANum 6))
>>> putStr $ iDisplay $ pprintAnnExpr (iStr . id) (iStr . show) 0 expr
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
        altsDoc = iInterleave iNewline (map (pprintAnnAlt ppra pprb (d + 1)) alts)
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
