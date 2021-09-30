{-# LANGUAGE NamedFieldPuns #-}
module Stack
  ( Stack         -- type only
    -- constructor
  , initStack
  , fromList
    -- getter
  , getStack      -- readonly
  , getWaterMark  -- readonly
  , getDepth
    -- operator
  , push
  , pop
  , pops
  ) where

data Stack a = Stack { stack :: [a]
                     , waterMark :: Int
                     }

-- | NOTE: record syntax で構成できないようにアクセサを別途あつらえる
getStack :: Stack a -> [a]
getStack = stack

-- | NOTE: record syntax で構成できないようにアクセサを別途あつらえる
getWaterMark :: Stack a -> Int
getWaterMark = waterMark

{- |
>>> stack initStack
[]

>>> waterMark initStack
0
-}
initStack :: Stack a
initStack = Stack [] 0

{- |
>>> stack $ fromList []
[]

>>> waterMark $ fromList []
0

>>> stack $ fromList [1..5]
[1,2,3,4,5]

>>> waterMark $ fromList [1..5]
5
-}
fromList :: [a] -> Stack a
fromList = Stack <*> length

{- |
>>> getDepth $ fromList []
0

>>> getDepth $ fromList [1..5]
5
-}
getDepth :: Stack a -> Int
getDepth = length . stack

{- |
>>> let s0 = initStack
>>> let s1 = push s0 "a"
>>> stack s1
["a"]
>>> waterMark s1
1

>>> let s2 = push s1 "b"
>>> stack s2
["b","a"]
>>> waterMark s2
2
-}
push :: Stack a -> a -> Stack a
push s x = s { stack = stack'
             , waterMark = waterMark'
             }
  where stack'     = x : stack s
        waterMark' = max (length stack') (waterMark s)

{- |
>>> let s0 = fromList [1..5]
>>> let (x, s1) = pop s0
>>> x
1
>>> stack s1
[2,3,4,5]
>>> waterMark s1
5

>>> let (y, s2) = pop s1
>>> y
2
>>> stack s2
[3,4,5]
>>> waterMark s2
5

>>> let (z, s3) = pop s2
>>> z
3
>>> stack s3
[4,5]
>>> waterMark s3
5
-}
pop :: Stack a -> (a, Stack a)
pop s = case stack s of
  [] -> error "Empty stack"
  (x:stack') -> (x, s { stack = stack' })


{- |
>>> let s0 = fromList [1..10]
>>> let (xs, s1) = pops s0 4
>>> xs
[1,2,3,4]
>>> stack s1
[5,6,7,8,9,10]
>>> waterMark s1
10

>>> let (ys, s2) = pops s1 1
>>> ys
[5]
>>> stack s2
[6,7,8,9,10]
>>> waterMark s2
10
-}
pops :: Stack a -> Int -> ([a], Stack a)
pops s n = (xs, s { stack = rest })
  where
    (xs, rest) = splitAt n $ stack s
