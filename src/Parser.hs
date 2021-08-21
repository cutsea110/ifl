module Parser
  ( Location
  , Token
  , Parser
  , pSat
  , pLit
  , pNum
  , pAlt
  , pThen, pThen3, pThen4
  , pZeroOrMore, pOneOrMore, pOneOrMoreWithSep
  , pEmpty
  , pApply
  , (<$$>), (<**>)
  , (<**), (**>)
  ) where

import Data.Char (isAlpha, isDigit)

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
    ap pf pa toks = [ (f v, toks2)
                    | (f, toks1) <- pf toks
                    , (v, toks2) <- pa toks1
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
[(["a","b"],[]),(["a"],[(1,","),(1,"b")])]

>>> pOneOrMoreWithSep pVar (pLit ",") [(1, "a"), (1, ","), (1, "b"), (1, ",")]
[(["a","b"],[(1,",")]),(["a"],[(1,","),(1,"b"),(1,",")])]

>>> pOneOrMoreWithSep pVar (pLit ",") [(1, "a"), (1, ","), (1, "b"), (1, ","), (1, "c")]
[(["a","b","c"],[]),(["a","b"],[(1,","),(1,"c")]),(["a"],[(1,","),(1,"b"),(1,","),(1,"c")])]

>>> pOneOrMoreWithSep pVar (pLit ",") [(1, "a"), (1, ","), (1, "b"), (1, "c"), (1, "d")]
[(["a","b"],[(1,"c"),(1,"d")]),(["a"],[(1,","),(1,"b"),(1,"c"),(1,"d")])]
-}
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p sep = (:) <$$> p <**> pZeroOrMore (sep **> p)

---------------------------------------
-- for Test's pVar (don't export)
---------------------------------------

pVar :: Parser String
pVar = pSat p
  where p cs@(c:_) = isAlpha c

pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = pLit "hello" `pAlt` pLit "goodbye"

{- |
>>> pGreeting [(1,"goodbye"), (1,"James"), (1,"!")]
[(("goodbye","James"),[])]
-}
pGreeting :: Parser (String, String)
pGreeting = pThen3 mkGreeting pHelloOrGoodbye pVar (pLit "!")
  where mkGreeting hg name _exclamation = (hg, name)
