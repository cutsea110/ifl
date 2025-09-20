module Annotated where

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
>>> iDisplay $ pprintAnnExpr (iStr . id) (iStr . show) 0 expr
{- () -} x

>>> let expr = ((), ALam ["y"] ((), AVar "y"))
>>> iDisplay $ pprintAnnExpr (iStr . id) (iStr . show) 0 expr
{- () -} \ y -> {- () -} y

>>> let expr = ((), ACase ((), AVar "x") [(1, ["y"], ((), AVar "y")), (2, ["z"], ((), ANum 3))])
>>> iDisplay $ pprintAnnExpr (iStr . id) (iStr . show) 0 expr
{- () -} case {- () -} x of
  <1> y -> {- () -} y
  <2> z -> {- () -} 3

>>> let expr = ((), ALet False [("x", ((), ANum 1)), ("y", ((), ANum 2))] ((), AVar "x"))
>>> iDisplay $ pprintAnnExpr (iStr . id) (iStr . show) 0 expr
{- () -} let
  x = {- () -} 1;
  y = {- () -} 2
in
  {- () -} x

>>> let expr = ((), ALet True [("x", ((), ANum 1)), ("y", ((), ANum 2))] ((), AVar "x"))
>>> iDisplay $ pprintAnnExpr (iStr . id) (iStr . show) 0 expr
{- () -} letrec
  x = {- () -} 1;
  y = {- () -} 2
in
  {- () -} x

>>> let expr = ((), AAp ((), AAp ((), AVar "f") ((), AVar "x")) ((), AVar "y"))
>>> iDisplay $ pprintAnnExpr (iStr . id) (iStr . show) 0 expr
{- () -} f {- () -} x {- () -} y

-- 3 + 6
>>> let expr = ((), AAp ((), AAp ((), AVar "+") ((), ANum 3)) ((), ANum 6))
>>> iDisplay $ pprintAnnExpr (iStr . id) (iStr . show) 0 expr
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
    in iConcat [ iStr (space (d * 2)), iStr "{- ", iNum tag, iStr " -} ", argsDoc
               , iStr " -> ", exprDoc
               ]
