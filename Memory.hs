module Memory where

type Memory a = [a]

type MemoryZipper a = (Memory a, Memory a)

zipped :: Memory a -> MemoryZipper a
zipped mem = (mem, [])

next :: MemoryZipper a -> MemoryZipper a
next (x:xs, ys) = (xs, x:ys) 

previous :: MemoryZipper a -> MemoryZipper a
previous (xs, y:ys) = (y:xs, ys)

apply :: (a -> a) -> MemoryZipper a -> MemoryZipper a
apply f (x:xs, ys) = (f x : xs, ys)

cell :: MemoryZipper a -> a
cell (x:xs, _) = x
