module Parser
  ( Location
  , Token
  , Parser
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
  , (<$$>), (<$$)
  , (<**>), (<**), (**>)
  ) where

import Data.Char (isAlpha, isDigit)

type Location = Int
type Token = (Location, String)
type Parser a = [Token] -> [(a, [Token])]

{- |
>>> pSat (=="a") []
[]

>>> pSat (=="a") [(1, "b")]
[]

>>> pSat (=="a") [(1, "a")]
[("a",[])]

>>> pSat (all isAlpha) [(1, "42")]
[]

>>> pSat (all isAlpha) [(1, "a1")]
[]

>>> pSat (all isAlpha) [(1, "abc")]
[("abc",[])]

>>> pSat (all isAlpha) [(1, "HELLO")]
[("HELLO",[])]

>>> pSat (all isAlpha) [(1, "abc42xyz")]
[]
-}
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

>>> pLit "hello" `pAlt` (pSat (all isAlpha)) $ [(1,"hello")]
[("hello",[]),("hello",[])]

>>> ("bye" <$$ pLit "hello") `pAlt` (pSat (all isAlpha)) $ [(1,"hello")]
[("bye",[]),("hello",[])]
-}
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks


{- |
>>> pLit "hello" `pAltL` (pSat (all isAlpha)) $ []
[]

>>> pLit "hello" `pAltL` (pSat (all isAlpha)) $ [(1,"hello")]
[("hello",[])]

>>> ("bye" <$$ pLit "hello") `pAltL` (pSat (all isAlpha)) $ [(1,"hello")]
[("bye",[])]
-}
pAltL :: Parser a -> Parser a -> Parser a
pAltL p1 p2 toks = p1 toks <+ p2 toks

infixr 3 `pAlt`, `pAltL`

(<+) :: [a] -> [a] -> [a]
[] <+ ys = ys
xs <+ _  = xs

infixr 5 <+

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

>>> pZeroOrMore (pLit "x") [(1, "x"), (1, "x"), (2, "x")]
[(["x","x","x"],[]),(["x","x"],[(2,"x")]),(["x"],[(1,"x"),(2,"x")]),([],[(1,"x"),(1,"x"),(2,"x")])]
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

>>> pOneOrMore (pLit "x") [(1, "x"), (1, "x"), (2, "x")]
[(["x","x","x"],[]),(["x","x"],[(2,"x")]),(["x"],[(1,"x"),(2,"x")])]
-}
pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

{- |
>>> pVar `pApply` (++"!") $ [(1, "a"), (1, "b"), (1, "c")]
[("a!",[(1,"b"),(1,"c")])]

>>> (pOneOrMore pVar) `pApply` (map (++"!")) $ [(1, "a"), (1, "b"), (1, "c")]
[(["a!","b!","c!"],[]),(["a!","b!"],[(1,"c")]),(["a!"],[(1,"b"),(1,"c")])]
-}
pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks = [(f v1, toks1) | (v1, toks1) <- p toks]

infixl 4 <$$>, <$$, <**>, <**, **>

{- |
>>> (++"!") <$$> pLit "a" $ []
[]

>>> (++"!") <$$> pLit "a" $ [(1, "a")]
[("a!",[])]

>>> (++"!") <$$> pLit "a" $ [(1, "b")]
[]

>>> (*3) <$$> pNum $ [(1, "14")]
[(42,[])]
-}
(<$$>) :: (a -> b) -> Parser a -> Parser b
(<$$>) = flip pApply

{- |
>>> 1 <$$ pLit "True" $ [(1, "True")]
[(1,[])]

>>> 0 <$$ pLit "False" $ [(1, "False")]
[(0,[])]

>>> 0 <$$ pLit "True" $ []
[]

>>> 1 <$$ pLit "True" $ [(1, "Hello")]
[]
-}
(<$$) :: a -> Parser b -> Parser a
v <$$ px =  const v <$$> px

(<**>) :: Parser (a -> b) -> Parser a -> Parser b
(<**>) = ap
  where
    ap pf px toks = [ (f v, toks2)
                    | (f, toks1) <- pf toks
                    , (v, toks2) <- px toks1
                    ]

{- |
>>> (pLit "a") <** (pLit "b") $ []
[]

>>> (pLit "a") <** (pLit "b") $ [(1, "a")]
[]

>>> (pLit "a") <** (pLit "b") $ [(1, "b")]
[]

>>> (pLit "a") <** (pLit "b") $ [(1, "a"), (1, "b")]
[("a",[])]

>>> (pLit "a") <** (pLit "b") $ [(1, "a"), (1, "b"),(1, "c")]
[("a",[(1,"c")])]
-}
(<**) :: Parser a -> Parser b -> Parser a
(<**) = pThen const

{- |
>>> (pLit "a") **> (pLit "b") $ []
[]

>>> (pLit "a") **> (pLit "b") $ [(1, "a")]
[]

>>> (pLit "a") **> (pLit "b") $ [(1, "b")]
[]

>>> (pLit "a") **> (pLit "b") $ [(1, "a"), (1, "b")]
[("b",[])]

>>> (pLit "a") **> (pLit "b") $ [(1, "a"), (1, "b"), (1, "c")]
[("b",[(1,"c")])]
-}
(**>) :: Parser a -> Parser b -> Parser b
(**>) = pThen (flip const)

{- |
>>> pOneOrMoreWithSep pVar (pLit ",") []
[]

>>> pOneOrMoreWithSep pVar (pLit ",") [(1, "a")]
[(["a"],[])]

>>> pOneOrMoreWithSep pVar (pLit ",") [(1, "a"), (1, ",")]
[(["a"],[(1,",")])]

>>> pOneOrMoreWithSep pVar (pLit ",") [(1, "a"), (1, ","), (1, "b")]
[(["a","b"],[])]

>>> pOneOrMoreWithSep pVar (pLit ",") [(1, "a"), (1, ","), (1, "b"), (1, ",")]
[(["a","b"],[(1,",")])]

>>> pOneOrMoreWithSep pVar (pLit ",") [(1, "a"), (1, ","), (1, "b"), (1, ","), (1, "c")]
[(["a","b","c"],[])]

>>> pOneOrMoreWithSep pVar (pLit ",") [(1, "a"), (1, ","), (1, "b"), (1, "c"), (1, "d")]
[(["a","b"],[(1,"c"),(1,"d")])]
-}
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p sep = (:) <$$> p <**> pMunch (sep **> p)

{- |
>>> pMunch (pLit "Hello") [(1, "Hello"), (1, "Bye")]
[(["Hello"],[(1,"Bye")])]

>>> pMunch (pLit "Hello") [(1, "Hellow"), (1, "Bye~")]
[([],[(1,"Hellow"),(1,"Bye~")])]

>>> pMunch (pLit "x") [(1, "x"), (1, "x"), (2, "x")]
[(["x","x","x"],[])]
-}
pMunch :: Parser a -> Parser [a]
pMunch p = pMunch1 p `pAltL` pEmpty []

{- |
>>> pMunch1 (pLit "Hello") [(1, "Hello"), (1, "Bye")]
[(["Hello"],[(1,"Bye")])]

>>> pMunch1 (pLit "Hello") [(1, "Hellow"), (1, "Bye~")]
[]

>>> pMunch1 (pLit "x") [(1, "x"), (1, "x"), (2, "x")]
[(["x","x","x"],[])]
-}
pMunch1 :: Parser a -> Parser [a]
pMunch1 p = pThen (:) p (pMunch p)

---------------------------------------
-- for Test's pVar (don't export)
---------------------------------------

{- |
>>> pVar [(1, "a")]
[("a",[])]

>>> pVar [(1, "42")]
[]

>>> pVar [(1, "a13")]
[("a13",[])]
-}
pVar :: Parser String
pVar = pSat p
  where p cs@(c:_) = isAlpha c

{- |
>>> pHelloOrGoodbye [(1, "howdy")]
[]

>>> pHelloOrGoodbye [(1, "hello")]
[("hello",[])]

>>> pHelloOrGoodbye [(1, "goodbye")]
[("goodbye",[])]
-}
pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = pLit "hello" `pAlt` pLit "goodbye"

{- |
>>> pGreeting [(1,"goodbye"), (1,"James"), (1,"!")]
[(("goodbye","James"),[])]
-}
pGreeting :: Parser (String, String)
pGreeting = pThen3 mkGreeting pHelloOrGoodbye pVar (pLit "!")
  where mkGreeting hg name _exclamation = (hg, name)
