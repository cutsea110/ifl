module Heap
  ( Size
  , Addr
  , Allocs
  , Heap
  , hInitial
  , hAlloc
  , hUpdate
  , hFree
  , hLookup
  , hAddresses
  , hSize
  , hNull
  , hIsnull
  , showaddr
  , remove
  ) where

import Utils (Assoc, aLookup)

type Size = Int
type Addr = Int
type Allocs = Int
type Heap a = (Allocs, Size, [Addr], Assoc Addr a)

hInitial :: Heap a
hInitial = (0, 0, [1..], [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (allocs, size, next:free, cts) n = ((allocs+1, size+1, free, (next, n):cts), next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (allocs, size, free, cts) a n = (allocs, size, free, (a, n):remove cts a)

hFree :: Heap a -> Addr -> Heap a
hFree (allocs, size, free, cts) a = (allocs, size-1, a:free, remove cts a)

hLookup :: Heap a -> Addr -> a
hLookup (allocs, size, free, cts) a
  = aLookup cts a (error ("can't find node " ++ showaddr a ++ " in heap"))

hAddresses :: Heap a -> [Addr]
hAddresses (allocs, size, free, cts) = [addr | (addr, node) <- cts]

hSize :: Heap a -> Size
hSize (_, size, _, _) = size

hNull :: Addr
hNull = 0

hIsnull :: Addr -> Bool
hIsnull a = a == 0

showaddr :: Addr -> String
showaddr a = "#" ++ show a

{- |
>>> remove [(1, 10)] 1
[]

>>> remove [(1, 10), (2, 20)] 1
[(2,20)]

>>> remove [(1, 10), (1, 11)] 1
[(1,11)]

>>> remove [(1, 10), (2, 20)] 2
[(1,10)]

>>> remove [(1, 10), (2, 20), (3, 30)] 2
[(1,10),(3,30)]
-}
remove :: [(Addr, a)] -> Addr -> [(Addr, a)]
remove [] a = error $ "Attempt to update or free nonexistent address: " ++ showaddr a
remove ((a', n):cts) a | a == a' = cts
                       | a /= a' = (a', n):remove cts a
