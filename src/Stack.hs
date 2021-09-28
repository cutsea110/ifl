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

type Stack a = ([a], Int)

-- | NOTE: record syntax で構成できないようにアクセサを別途あつらえる
getStack :: Stack a -> [a]
getStack = fst

-- | NOTE: record syntax で構成できないようにアクセサを別途あつらえる
getWaterMark :: Stack a -> Int
getWaterMark = snd

initStack :: Stack a
initStack = ([], 0)

fromList :: [a] -> Stack a
fromList xs = (xs, length xs)

getDepth :: Stack a -> Int
getDepth = length . fst

push :: Stack a -> a -> Stack a
push (xs, wm) x = (xs', wm')
  where xs' = x:xs
        wm' = max (length xs') wm

pop :: Stack a -> (a, Stack a)
pop ([], _)    = error "Empty stack"
pop (x:xs, wm) = (x, (xs, wm))

pops :: Stack a -> Int -> ([a], Stack a)
pops (xs, wm) n = (xs', (rest, wm))
  where
    (xs', rest) = splitAt n xs
