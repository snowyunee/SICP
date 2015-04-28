import Control.Monad
import DeepReverse


main = do
--  print $ "[[6,5,4],[321],[3,2,0]]"
  print $ ((deepReverse [[[7],[6,5]],[[444]],[[3],[2,1]]]) :: [[[Int]]])
  print $ ((deepReverse [[[4,3],[2, 1]]]) :: [[[Int]]])
  print $ ((deepReverse [[[2,1]]]) :: [[[Int]]])
  print $ ((deepReverse [[2,1]]) :: [[Int]])
  print $ ((deepReverse [1]) :: [Int])

