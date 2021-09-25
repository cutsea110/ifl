module Heap where

type Size = Int
type Addr = Int
type Heap a = (Size, [Addr], [(Addr, a)])

hInitial :: Heap a
hInitial = (0, [1..], [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, (next:free), cts) n = ((size+1, free, (next, n):cts), next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (size, free, cts) a n = (size, free, (a, n):remove cts a)

hFree :: Heap a -> Addr -> Heap a
hFree (size, free, cts) a = (size-1, a:free, remove cts a)

hSize :: Heap a -> Size
hSize (size, _, _) = size

hNull :: Addr
hNull = 0

hIsnull :: Addr -> Bool
hIsnull a = a == 0

showaddr :: Addr -> String
showaddr a = "#" ++ show a

remove :: [(Addr, a)] -> Addr -> [(Addr, a)]
remove [] a = error $ "Attempt tio update or free nonexistent address: " ++ showaddr a
remove ((a', n):cts) a | a == a' = cts
                       | a /= a' = (a', n):remove cts a
