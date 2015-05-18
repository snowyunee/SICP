import Control.Monad
--import Data.Tree

data Tree a = Node a | SubTree (Forest a)
--data Tree a = Node a | Forest a
  deriving Show

type Forest a = [Tree a]

squareTree :: Num a => Tree a -> Tree a
squareTree (Node x) = Node (x * x)
squareTree (SubTree l) = SubTree (map squareTree l)

-- squareTreeDirect :: Num a => Tree a -> Tree a
-- squareTreeDirect (Node t) = Node (t * t)
-- squareTreeDirect (SubTree (x:xs)) = SubTree ((squareTreeDirect x):(squareTreeDirect xs))

treeMap f (Node t) = Node (f t)
treeMap f (SubTree l) = SubTree (map (treeMap f) l)

square x = x * x

main = do
  let tree = SubTree [Node 1, SubTree [Node 2, SubTree [Node 3, Node 5], Node 5], SubTree [Node 6, Node 7]]
--  let tree = [Node 1, [Node 2, [Node 3, Node 5], Node 5], [Node 6, Node 7]] :: Tree Int
  print $ tree
  print $ squareTree tree
  print $ treeMap square tree

  --print $ squareTreeDirect tree

