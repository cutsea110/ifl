{-# LANGUAGE NamedFieldPuns #-}
module Stack
  ( Stack             -- type only
    -- constructor
  , emptyStack
  , fromList
    -- getter
  , getStack          -- readonly
  , getDepth          -- readonly
  , getHighWaterMark  -- readonly
  , isEmpty
    -- setter
  , setStack
    -- operator
  , push
  , pop
  , discard
  , restore
  ) where

data Stack a
  = Stack { stack :: [a]
          , depth :: Int
          , highWaterMark :: Int
          } deriving Show

{- |
>>> getStack $ fromList []
[]

>>> getStack $ fromList [1..5]
[1,2,3,4,5]
-}
-- | NOTE: record syntax で構成できないようにアクセサを別途あつらえる
getStack :: Stack a -> [a]
getStack = stack

-- | TODO: テスト書く
setStack :: Stack a -> [a] -> Stack a
setStack s xs = s { stack = xs
                  , highWaterMark = hwm
                  }
  where hwm = max (length xs) (highWaterMark s)

{- |
>>> getDepth $ fromList []
0

>>> getDepth $ fromList [1..5]
5
-}
-- | NOTE: record syntax で構成できないようにアクセサを別途あつらえる
getDepth :: Stack a -> Int
getDepth = depth

{- |
>>> getHighWaterMark $ fromList []
0

>>> getHighWaterMark $ fromList [1..5]
5
-}
-- | NOTE: record syntax で構成できないようにアクセサを別途あつらえる
getHighWaterMark :: Stack a -> Int
getHighWaterMark = highWaterMark

{- |
>>> isEmpty emptyStack
True

>>> isEmpty $ fromList [1]
False
-}
isEmpty :: Stack a -> Bool
isEmpty (Stack s _ _) = null s

{- |
>>> stack emptyStack
[]

>>> highWaterMark emptyStack
0
-}
emptyStack :: Stack a
emptyStack = Stack [] 0 0

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
>>> let s0 = emptyStack
>>> let s1 = push "a" s0
>>> stack s1
["a"]
>>> highWaterMark s1
1
>>> getDepth s1
1

>>> let s2 = push "b" s1
>>> stack s2
["b","a"]
>>> highWaterMark s2
2
>>> getDepth s2
2
-}
push :: a -> Stack a -> Stack a
push x s = s { stack = stack'
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

{- |
>>> let s0 = fromList [1..2]
>>> highWaterMark s0
2
>>> let d0 = fromList []
>>> let d1 = push s0 d0
>>> let s1 = fromList [1..5]
>>> highWaterMark s1
5
>>> let (s2, d2) = restore s1 d1
>>> highWaterMark s2
5
>>> depth s2
2
>>> stack s2
[1,2]
-}
restore :: Stack a -> Stack (Stack a) -> (Stack a, Stack (Stack a))
restore s ss = (s', ss')
  where (s1, ss') = pop ss
        s' = s1 { highWaterMark = max hwm1 hwm2 }
        (hwm1, hwm2) = (highWaterMark s1, highWaterMark s)
