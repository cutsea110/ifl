{-# LANGUAGE NamedFieldPuns #-}
module Stack
  ( Stack         -- type only
  , getStack      -- readonly
  , getWaterMark  -- readonly
  , getDepth
  , initStack
  , fromList
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

initStack :: Stack a
initStack = Stack [] 0

fromList :: [a] -> Stack a
fromList = Stack <*> length

getDepth :: Stack a -> Int
getDepth = length . stack

push :: Stack a -> a -> Stack a
push s@(Stack stack waterMark) x
  = s { stack = stack'
      , waterMark = waterMark'
      }
  where stack'     = x : stack
        waterMark' = max (length stack') waterMark

pop :: Stack a -> (a, Stack a)
pop s@(Stack stack waterMark) = case stack of
  [] -> error "Empty stack"
  (x:stack') -> (x, s { stack = stack' })

pops :: Stack a -> Int -> ([a], Stack a)
pops s n = (xs, s { stack = rest })
  where
    (xs, rest) = splitAt n $ stack s
