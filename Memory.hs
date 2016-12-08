module Memory where

type Memory a = [a]

type MemoryZipper a = (Memory a, Memory a)

zipped :: Memory a -> MemoryZipper a
zipped mem = (mem, [])

next :: MemoryZipper a -> MemoryZipper a
next (x:xs, ys) = (xs, x:ys) 

previous :: MemoryZipper a -> MemoryZipper a
previous (xs, y:ys) = (y:xs, ys)

updateCell :: (a -> a) -> MemoryZipper a -> MemoryZipper a
updateCell f (x:xs, ys) = (f x : xs, ys)

updateCellWithValue :: a -> MemoryZipper a -> MemoryZipper a
updateCellWithValue val = updateCell (const val)

cell :: MemoryZipper a -> a
cell (x:xs, _) = x
