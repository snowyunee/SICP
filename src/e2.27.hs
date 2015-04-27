import Control.Monad

reverse' :: [a] -> [a]
reverse' xs = reverse'' xs []
  where reverse'' []      a = a
        reverse'' (x:xs)  a = reverse'' xs (x:a)

deepReverse :: [a] -> [a]
deepReverse xs@[]       = xs
deepReverse xs@(_:_)    = reverse' $ map deepReverse xs
deepReverse xs          = xs

main = do
  print $ "[[6,5,4],[321],[3,2,0]]"
  print $ deepReverse [[[0],[2,3]],[[321]],[[4],[5,6]]]
  print $ deepReverse [1,2,3]
  print $ deepReverse [1]
  print $ deepReverse [[1,2,3]]

