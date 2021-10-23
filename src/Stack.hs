{-# LANGUAGE NamedFieldPuns #-}
module Stack
  ( Stack         -- type only
    -- constructor
  , initStack
  , fromList
    -- getter
  , getStack      -- readonly
  , getDepth      -- readonly
  , getWaterMark  -- readonly
    -- operator
  , push
  , pop
  , discard
  ) where

data Stack a = Stack { stack :: [a]
                     , depth :: Int
                     , highWaterMark :: Int
                     }

-- | NOTE: record syntax で構成できないようにアクセサを別途あつらえる
getStack :: Stack a -> [a]
getStack (Stack s _ _) = s

-- | NOTE: record syntax で構成できないようにアクセサを別途あつらえる
getWaterMark :: Stack a -> Int
getWaterMark (Stack _ _ m) = m

{- |
>>> stack initStack
[]

>>> highWaterMark initStack
0
-}
initStack :: Stack a
initStack = Stack [] 0 0

{- |
>>> stack $ fromList []
[]

>>> depth $ fromList []
0

>>> highWaterMark $ fromList []
0

>>> stack $ fromList [1..5]
[1,2,3,4,5]

>>> highWaterMark $ fromList [1..5]
5
-}
fromList :: [a] -> Stack a
fromList xs = Stack xs l l
  where l = length xs

{- |
>>> getDepth $ fromList []
0

>>> getDepth $ fromList [1..5]
5
-}
getDepth :: Stack a -> Int
getDepth (Stack _ d _) = d

{- |
>>> let s0 = initStack
>>> let s1 = push s0 "a"
>>> stack s1
["a"]
>>> highWaterMark s1
1
>>> getDepth s1
1

>>> let s2 = push s1 "b"
>>> stack s2
["b","a"]
>>> highWaterMark s2
2
>>> getDepth s2
2
-}
push :: Stack a -> a -> Stack a
push s x = s { stack = stack'
             , depth = depth'
             , highWaterMark = highWaterMark'
             }
  where stack'         = x : stack s
        depth'         = depth s + 1
        highWaterMark' = max depth' (highWaterMark s)

{- |
>>> let s0 = fromList [1..5]
>>> let (x, s1) = pop s0
>>> x
1
>>> stack s1
[2,3,4,5]
>>> highWaterMark s1
5
>>> getDepth s1
4

>>> let (y, s2) = pop s1
>>> y
2
>>> stack s2
[3,4,5]
>>> highWaterMark s2
5
>>> getDepth s2
3

>>> let (z, s3) = pop s2
>>> z
3
>>> stack s3
[4,5]
>>> highWaterMark s3
5
>>> getDepth s3
2
-}
pop :: Stack a -> (a, Stack a)
pop s = case stack s of
  [] -> error "Empty stack"
  (x:stack') -> (x, s { stack = stack', depth = depth' })
    where depth' = depth s - 1


{- |
>>> let s0 = fromList [1..10]
>>> let s1 = discard 4 s0
>>> stack s1
[5,6,7,8,9,10]
>>> highWaterMark s1
10
>>> getDepth s1
6

>>> let s2 = discard 1 s1
>>> stack s2
[6,7,8,9,10]
>>> highWaterMark s2
10
>>> getDepth s2
5
-}
discard :: Int -> Stack a -> Stack a
discard n s = s { stack = stack', depth = depth' }
  where
    stack' = drop n $ stack s
    depth' = max 0 (depth s - n)