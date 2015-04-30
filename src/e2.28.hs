import Control.Monad
import Flatten

--fringe = flatten

main = do
  print $ ((flatten ([[[7],[6,5]],[[444]],[[3],[2,1]]] :: [[[Int]]])) :: [Int])
  print $ ((flatten ([[[4,3],[2, 1]]] :: [[[Int]]])) :: [Int])
  print $ ((flatten ([[[2,1]]] :: [[[Int]]])) :: [Int])
  print $ ((flatten ([[2,1]] :: [[Int]])) :: [Int])
  print $ ((flatten ([1] :: [Int])) :: [Int])
  
