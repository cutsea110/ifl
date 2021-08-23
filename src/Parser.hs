module Parser
  ( Location
  , Token
  , Parser (runParser)
  , pSat
  , pLit
  , pNum
  , pAlt
  , pAltL
  , pThen, pThen3, pThen4
  , pZeroOrMore, pOneOrMore, pOneOrMoreWithSep
  , pMunch, pMunch1
  , pEmpty
  , pApply
  ) where

import Control.Applicative
import Data.Char (isAlpha, isDigit)

type Location = Int
type Token = (Location, String)
newtype Parser a = Parser { runParser :: [Token] -> [(a, [Token])] }

instance Functor Parser where
  fmap = flip pApply

instance Applicative Parser where
  pure = pEmpty
  (<*>) = pAp

instance Monad Parser where
  (>>=) = pBind

{- |
>>> runParser (pSat (=="a")) []
[]

>>> runParser (pSat (=="a")) [(1, "b")]
[]

>>> runParser (pSat (=="a")) [(1, "a")]
[("a",[])]

>>> runParser (pSat (all isAlpha)) [(1, "42")]
[]

>>> runParser (pSat (all isAlpha)) [(1, "a1")]
[]

>>> runParser (pSat (all isAlpha)) [(1, "abc")]
[("abc",[])]

>>> runParser (pSat (all isAlpha)) [(1, "HELLO")]
[("HELLO",[])]

>>> runParser (pSat (all isAlpha)) [(1, "abc42xyz")]
[]
-}
pSat :: (String -> Bool) -> Parser String
pSat pred = Parser f
  where
    f ((_, tok):toks)
      | pred tok  = [(tok, toks)]
      | otherwise = []
    f []     = []

{- |
>>> runParser (pLit "a") []
[]

>>> runParser (pLit "a") [(1,"a")]
[("a",[])]

>>> runParser (pLit "a") [(1,"b")]
[]

>>> runParser (pLit "a") [(1,"b"),(1,"a")]
[]
-}
pLit :: String -> Parser String
pLit s = pSat (s==)

{- |
>>> runParser pNum []
[]

>>> runParser pNum [(1, "a")]
[]

>>> runParser pNum [(1, "42")]
[(42,[])]

>>> runParser pNum [(1, "4a")]
[]
-}
pNum :: Parser Int
pNum = pSat (all isDigit) `pApply` read


{- |
>>> runParser (pLit "Hello" `pAlt` pLit "Bye") []
[]

>>> runParser (pLit "Hello" `pAlt` pLit "Bye") [(1,"a")]
[]

>>> runParser (pLit "Hello" `pAlt` pLit "Bye") [(1,"Hello")]
[("Hello",[])]

>>> runParser (pLit "Hello" `pAlt` pLit "Bye") [(1,"Bye")]
[("Bye",[])]

>>> runParser (pLit "Hello" `pAlt` pLit "Bye") [(1,"Hellow")]
[]

>>> runParser (pLit "Hello" `pAlt` pLit "Bye") [(1,"Hellow"),(1,"Bye")]
[]

>>> runParser (pLit "Hello" `pAlt` pLit "Bye") [(1,"Bye"),(1,"Hello")]
[("Bye",[(1,"Hello")])]

>>> runParser (pLit "hello" `pAlt` (pSat (all isAlpha))) [(1,"hello")]
[("hello",[]),("hello",[])]

>>> runParser (("bye" <$ pLit "hello") `pAlt` (pSat (all isAlpha))) [(1,"hello")]
[("bye",[]),("hello",[])]
-}
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 = Parser (\toks -> runParser p1 toks ++ runParser p2 toks)


{- |
>>> runParser (pLit "hello" `pAltL` (pSat (all isAlpha))) []
[]

>>> runParser (pLit "hello" `pAltL` (pSat (all isAlpha))) [(1,"hello")]
[("hello",[])]

>>> runParser (("bye" <$ pLit "hello") `pAltL` (pSat (all isAlpha))) [(1,"hello")]
[("bye",[])]
-}
pAltL :: Parser a -> Parser a -> Parser a
pAltL p1 p2 = Parser (\toks -> runParser p1 toks <+ runParser p2 toks)

infixr 3 `pAlt`, `pAltL`

(<+) :: [a] -> [a] -> [a]
[] <+ ys = ys
xs <+ _  = xs

infixr 5 <+

{- |
>>> runParser (pThen (++) (pLit "Hello") (pLit "World")) []
[]

>>> runParser (pThen (++) (pLit "Hello") (pLit "World")) [(1, "World")]
[]

>>> runParser (pThen (++) (pLit "Hello") (pLit "World")) [(1, "Hello"), (1, "World")]
[("HelloWorld",[])]
-}
pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen = liftA2
{-
pThen combine p1 p2
  = Parser (\toks -> [ (combine v1 v2, toks2)
                     | (v1, toks1) <- runParser p1 toks
                     , (v2, toks2) <- runParser p2 toks1
                     ])
-}

{- |
>>> runParser (pThen3 (\x y z -> x ++ y ++ z) (pLit "Hello ") pVar (pLit " san")) []
[]

>>> runParser (pThen3 (\x y z -> x ++ y ++ z) (pLit "Hello ") pVar (pLit " san")) [(1, "Hello "), (1, "cutsea"), (1, " san")]
[("Hello cutsea san",[])]

>>> runParser (pThen3 (\x y z -> x ++ y ++ z) (pLit "Hello ") pVar (pLit " san")) [(1, "Hello "), (1, "cutsea"), (1, " kun")]
[]
-}
pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 = liftA3
{-
pThen3 combine p1 p2 p3
  = Parser (\toks -> [ (v2, toks2)
                     | (v1, toks1) <- runParser p1 toks
                     , (v2, toks2) <- runParser (pThen (combine v1) p2 p3) toks1
                     ])
-}

{- |
>>> runParser (pThen4 (,,,) pVar pVar pVar pVar) []
[]

>>> runParser (pThen4 (,,,) pVar pVar pVar pVar) [(1, "a"), (1, "b")]
[]

>>> runParser (pThen4 (,,,) pVar pVar pVar pVar) [(1, "a"), (1, "b"), (1, "c")]
[]

>>> runParser (pThen4 (,,,) pVar pVar pVar pVar) [(1, "a"), (1, "b"), (1, "c"), (1, "d")]
[(("a","b","c","d"),[])]

>>> runParser (pThen4 (,,,) pVar pVar pVar pVar) [(1, "a"), (1, "b"), (1, "c"), (1, "d"), (1, "e")]
[(("a","b","c","d"),[(1,"e")])]
-}
pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4
  = Parser (\toks -> [ (v2, toks2)
                     | (v1, toks1) <- runParser p1 toks
                     , (v2, toks2) <- runParser (pThen3 (combine v1) p2 p3 p4) toks1
                     ])
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
>>> runParser (pZeroOrMore (pLit "Hello")) [(1, "Hello"), (1, "Bye")]
[(["Hello"],[(1,"Bye")]),([],[(1,"Hello"),(1,"Bye")])]

>>> runParser (pZeroOrMore (pLit "Hello")) [(1, "Hellow"), (1, "Bye~")]
[([],[(1,"Hellow"),(1,"Bye~")])]

>>> runParser (pZeroOrMore (pLit "x")) [(1, "x"), (1, "x"), (2, "x")]
[(["x","x","x"],[]),(["x","x"],[(2,"x")]),(["x"],[(1,"x"),(2,"x")]),([],[(1,"x"),(1,"x"),(2,"x")])]
-}
pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pAlt` pEmpty []

{- |
>>> runParser (pEmpty []) []
[([],[])]

>>> runParser (pEmpty 1) []
[(1,[])]

>>> runParser (pEmpty "hello") [(1, "a")]
[("hello",[(1,"a")])]
-}
pEmpty :: a -> Parser a
pEmpty x = Parser (\toks -> [(x, toks)])

{- |
>>> runParser (pOneOrMore (pLit "Hello")) [(1, "Hello"), (1, "Bye")]
[(["Hello"],[(1,"Bye")])]

>>> runParser (pOneOrMore (pLit "Hello")) [(1, "Hellow"), (1, "Bye~")]
[]

>>> runParser (pOneOrMore (pLit "x")) [(1, "x"), (1, "x"), (2, "x")]
[(["x","x","x"],[]),(["x","x"],[(2,"x")]),(["x"],[(1,"x"),(2,"x")])]
-}
pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = (:) <$> p <*> pZeroOrMore p

{- |
>>> runParser (pVar `pApply` (++"!")) [(1, "a"), (1, "b"), (1, "c")]
[("a!",[(1,"b"),(1,"c")])]

>>> runParser ((pOneOrMore pVar) `pApply` (map (++"!"))) [(1, "a"), (1, "b"), (1, "c")]
[(["a!","b!","c!"],[]),(["a!","b!"],[(1,"c")]),(["a!"],[(1,"b"),(1,"c")])]
-}
pApply :: Parser a -> (a -> b) -> Parser b
pApply p f = Parser (\toks ->
                       [ (f v1, toks1)
                       | (v1, toks1) <- runParser p toks
                       ])

{- |
>>> runParser ((++"!") <$> pLit "a") []
[]

>>> runParser ((++"!") <$> pLit "a") [(1, "a")]
[("a!",[])]

>>> runParser ((++"!") <$> pLit "a") [(1, "b")]
[]

>>> runParser ((*3) <$> pNum) [(1, "14")]
[(42,[])]
-}
-- Functor
-- (<$>) :: (a -> b) -> Parser a -> Parser b
-- (<$>) = fmap

{- |
>>> runParser (1 <$ pLit "True") [(1, "True")]
[(1,[])]

>>> runParser (0 <$ pLit "False") [(1, "False")]
[(0,[])]

>>> runParser (0 <$ pLit "True") []
[]

>>> runParser (1 <$ pLit "True") [(1, "Hello")]
[]
-}
-- Functor
-- (<$) :: a -> Parser b -> Parser a
-- v <$ px = const v <$> px

{- |
>>> runParser (pure (+3) `pAp` pNum) []
[]

>>> runParser (pure (+3) `pAp` pNum) [(1,"39")]
[(42,[])]

>>> runParser (pure (\x y z -> x+y+z) `pAp` pNum `pAp` pNum `pAp` pNum) []
[]

>>> runParser (pure (\x y z -> x+y+z) `pAp` pNum `pAp` pNum `pAp` pNum) [(1,"1")]
[]

>>> runParser (pure (\x y z -> x+y+z) `pAp` pNum `pAp` pNum `pAp` pNum) [(1,"1"),(1,"2")]
[]

>>> runParser (pure (\x y z -> x+y+z) `pAp` pNum `pAp` pNum `pAp` pNum) [(1,"1"),(1,"2"),(1,"3")]
[(6,[])]

>>> runParser (pure (\x y z -> x+y+z) `pAp` pNum `pAp` pNum `pAp` pNum) [(1,"1"),(1,"2"),(1,"3"),(1,"4")]
[(6,[(1,"4")])]
-}
pAp :: Parser (a -> b) -> Parser a -> Parser b
pAp pf px = Parser (\toks ->
                       [ (f v, toks2)
                       | (f, toks1) <- runParser pf toks
                       , (v, toks2) <- runParser px toks1
                       ])

{- |
>>> runParser (pNum `pBind` (\x -> pure (x+3))) []
[]

>>> runParser (pNum `pBind` (\x -> pure (x+3))) [(1,"39")]
[(42,[])]

>>> runParser (pNum `pBind` (\x -> pure (2*x)) `pBind` (\x -> pure (x+3))) [(1,"7")]
[(17,[])]
-}
pBind :: Parser a -> (a -> Parser b) -> Parser b
pBind px f = Parser (\toks ->
                       [ (v2, toks2)
                       | (v1, toks1) <- runParser px toks
                       , (v2, toks2) <- runParser (f v1) toks1
                       ])

{- |
>>> runParser ((pLit "a") <* (pLit "b")) []
[]

>>> runParser ((pLit "a") <* (pLit "b")) [(1, "a")]
[]

>>> runParser ((pLit "a") <* (pLit "b")) [(1, "b")]
[]

>>> runParser ((pLit "a") <* (pLit "b")) [(1, "a"), (1, "b")]
[("a",[])]

>>> runParser ((pLit "a") <* (pLit "b")) [(1, "a"), (1, "b"),(1, "c")]
[("a",[(1,"c")])]
-}
-- Applicative
-- (<*) :: Parser a -> Parser b -> Parser a
-- (<*) = pThen const

{- |
>>> runParser ((pLit "a") *> (pLit "b")) []
[]

>>> runParser ((pLit "a") *> (pLit "b")) [(1, "a")]
[]

>>> runParser ((pLit "a") *> (pLit "b")) [(1, "b")]
[]

>>> runParser ((pLit "a") *> (pLit "b")) [(1, "a"), (1, "b")]
[("b",[])]

>>> runParser ((pLit "a") *> (pLit "b")) [(1, "a"), (1, "b"), (1, "c")]
[("b",[(1,"c")])]
-}
-- Applicative
-- (*>) :: Parser a -> Parser b -> Parser b
-- (*>) = pThen (flip const)

{- |
>>> runParser (pOneOrMoreWithSep pVar (pLit ",")) []
[]

>>> runParser (pOneOrMoreWithSep pVar (pLit ",")) [(1, "a")]
[(["a"],[])]

>>> runParser (pOneOrMoreWithSep pVar (pLit ",")) [(1, "a"), (1, ",")]
[(["a"],[(1,",")])]

>>> runParser (pOneOrMoreWithSep pVar (pLit ",")) [(1, "a"), (1, ","), (1, "b")]
[(["a","b"],[])]

>>> runParser (pOneOrMoreWithSep pVar (pLit ",")) [(1, "a"), (1, ","), (1, "b"), (1, ",")]
[(["a","b"],[(1,",")])]

>>> runParser (pOneOrMoreWithSep pVar (pLit ",")) [(1, "a"), (1, ","), (1, "b"), (1, ","), (1, "c")]
[(["a","b","c"],[])]

>>> runParser (pOneOrMoreWithSep pVar (pLit ",")) [(1, "a"), (1, ","), (1, "b"), (1, "c"), (1, "d")]
[(["a","b"],[(1,"c"),(1,"d")])]
-}
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p sep = (:) <$> p <*> pMunch (sep *> p)

{- |
>>> runParser (pMunch (pLit "Hello")) [(1, "Hello"), (1, "Bye")]
[(["Hello"],[(1,"Bye")])]

>>> runParser (pMunch (pLit "Hello")) [(1, "Hellow"), (1, "Bye~")]
[([],[(1,"Hellow"),(1,"Bye~")])]

>>> runParser (pMunch (pLit "x")) [(1, "x"), (1, "x"), (2, "x")]
[(["x","x","x"],[])]
-}
pMunch :: Parser a -> Parser [a]
pMunch p = pMunch1 p `pAltL` pEmpty []

{- |
>>> runParser (pMunch1 (pLit "Hello")) [(1, "Hello"), (1, "Bye")]
[(["Hello"],[(1,"Bye")])]

>>> runParser (pMunch1 (pLit "Hello")) [(1, "Hellow"), (1, "Bye~")]
[]

>>> runParser (pMunch1 (pLit "x")) [(1, "x"), (1, "x"), (2, "x")]
[(["x","x","x"],[])]
-}
pMunch1 :: Parser a -> Parser [a]
pMunch1 p = (:) <$> p <*> pMunch p

---------------------------------------
-- for Test's pVar (don't export)
---------------------------------------

{- |
>>> runParser pVar [(1, "a")]
[("a",[])]

>>> runParser pVar [(1, "42")]
[]

>>> runParser pVar [(1, "a13")]
[("a13",[])]
-}
pVar :: Parser String
pVar = pSat p
  where p cs@(c:_) = isAlpha c

{- |
>>> runParser pHelloOrGoodbye [(1, "howdy")]
[]

>>> runParser pHelloOrGoodbye [(1, "hello")]
[("hello",[])]

>>> runParser pHelloOrGoodbye [(1, "goodbye")]
[("goodbye",[])]
-}
pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = pLit "hello" `pAlt` pLit "goodbye"

{- |
>>> runParser pGreeting [(1,"Yes"), (1,"James"), (1,"!")]
[]

>>> runParser pGreeting [(1,"goodbye"), (1,"James"), (1,"!")]
[(("goodbye","James"),[])]
-}
pGreeting :: Parser (String, String)
pGreeting = pThen3 mkGreeting pHelloOrGoodbye pVar (pLit "!")
  where mkGreeting hg name _exclamation = (hg, name)
