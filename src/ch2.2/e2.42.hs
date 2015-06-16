
enumerateInterval l h
  | l > h       = []
  | otherwise   = [l..h]
  
safe k position = True

adjoinPosition newRow k restOfQueens = map 

queens boardSize = queenCols boardSize
  where
    queenCols k
      | k == 0 = []
      | otherwise =
          filter (\p -> safe k p) $
          concatMap (\restOfQeens -> map (\newRow -> adjoinPosition newRow k restOfQeens) enumerateInterval 1 boardSize) $
          queenCols (k - 1)

f n s =
  filter (\(x,y,z) -> s == x + y + z) $ makeTriple n

main = do
  print $ queens 4
