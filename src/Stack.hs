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
  , pop'
  ) where

data Stack a = Stack { stack :: [a]
                     , waterMark :: Int
                     }

-- | NOTE: record syntax で構成できないように
getStack :: Stack a -> [a]
getStack = stack

-- | NOTE: record syntax で構成できないように
getWaterMark :: Stack a -> Int
getWaterMark = waterMark

initStack :: Stack a
initStack = Stack [] 0

fromList :: [a] -> Stack a
fromList xs = Stack xs (length xs)

getDepth :: Stack a -> Int
getDepth s = length (stack s)

push :: Stack a -> a -> Stack a
push s@(Stack stack waterMark) x = s { stack = stack', waterMark = waterMark' }
  where stack' = x : stack
        waterMark' = max (length stack') waterMark

pop :: Stack a -> (a, Stack a)
pop s@(Stack stack waterMark) = case stack of
  [] -> error "Empty stack"
  (x:stack') -> (x, s { stack = stack' })

pop' :: Stack a -> Int -> ([a], Stack a)
pop' s n = (xs, s { stack = rest })
  where
    (xs, rest) = splitAt n $ stack s
