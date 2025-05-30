module Utils
  ( space
  , Assoc
  , aLookup
  , aDomain
  , aRange
  , aInsert
  , aDelete
  , aUpdate
  , fst3, snd3, thd3
  ) where

{- |
>>> space 0
""

>>> space 1
" "

>>> space 4
"    "

>>> space (-1)
""
-}
space :: Int -> String
space n = replicate n ' '

type Assoc a b = [(a, b)]

{- |
>>> aLookup [] 1 "missing"
"missing"

>>> aLookup [(1, "found")] 1 "not found"
"found"

>>> aLookup [(1, "found"), (2, "ok")] 2 "not found"
"ok"
-}
aLookup :: Eq k => Assoc k v -> k -> v -> v
aLookup [] _ d = d
aLookup ((k, v):kvs) k' d
  | k == k' = v
  | otherwise  = aLookup kvs k' d

aDomain :: Assoc k v -> [k]
aDomain = map fst

aRange :: Assoc k v -> [v]
aRange = map snd 

aInsert :: Eq k => Assoc k v -> k -> v -> Assoc k v
aInsert m k v = (k, v):m

aDelete :: Eq k => Assoc k v -> k -> Assoc k v
aDelete m k = filter ((/= k) . fst) m

aUpdate :: Eq k => Assoc k v -> k -> v -> Assoc k v
aUpdate m k v = (k, v):aDelete m k

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y
thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z
