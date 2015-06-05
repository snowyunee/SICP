module Tree where

data Tree a = Node a | SubTree (Forest a)
  deriving Show

type Forest a = [Tree a]

treeMap f (Node t) = Node (f t)
treeMap f (SubTree l) = SubTree (map (treeMap f) l)

