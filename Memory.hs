module Memory where

type Memory a = [a]

type MemoryZipper a = (a, Memory a, Memory a)

zipped :: Memory a -> MemoryZipper a
zipped mem = (head mem, tail mem, [])

next :: MemoryZipper a -> MemoryZipper a
next (c ,x:xs, ys) = (x, xs, c:ys) 

previous :: MemoryZipper a -> MemoryZipper a
previous (c, xs, y:ys) = (y, c:xs, ys)

updateCell :: (a -> a) -> MemoryZipper a -> MemoryZipper a
updateCell f (c, xs, ys) = (f c, xs, ys)

updateCellWithValue :: a -> MemoryZipper a -> MemoryZipper a
updateCellWithValue val = updateCell (const val)

cell :: MemoryZipper a -> a
cell (c, xs, ys) = c
