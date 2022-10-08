module Utils
  ( space
  , Assoc
  , aLookup
  , aDomain
  , aRange
  , aInsert
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
