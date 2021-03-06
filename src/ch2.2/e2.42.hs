
enumerateInterval :: (Ord a, Enum a) => a -> a -> [a]
enumerateInterval l h
  | l > h       = []
  | otherwise   = [l..h]
  

safe :: (Num a, Eq a) => a -> [(a,a)] -> Bool
safe k positions =
  foldr (check (last positions)) True (init positions)
    where check (xK,yK) (x,y) acc
            | acc == False            = False
            | yK == y                 = False
            | abs(x-xK) == abs(y-yK)  = False
            | otherwise               = True

adjoinPosition :: a -> a -> [(a,a)] -> [(a,a)]
adjoinPosition newRow k restOfQueens = restOfQueens ++ [(k, newRow)]


emptyBoard = [[]]


queens boardSize = queenCols boardSize
  where
    queenCols k
      | k == 0 = emptyBoard
      | otherwise =
          filter (\positions -> safe k positions) $
          concatMap (\restOfQeens -> 
                        map (\newRow -> adjoinPosition newRow k restOfQeens)
                            (enumerateInterval 1 boardSize)) $
          queenCols (k - 1)

main = do
  -- [[(1,2),(2,4),(3,6),(4,1),(5,3),(6,5)],
  -- [(1,3),(2,6),(3,2),(4,5),(5,1),(6,4)],
  -- [(1,4),(2,1),(3,5),(4,2),(5,6),(6,3)],
  -- [(1,5),(2,3),(3,1),(4,6),(5,4),(6,2)]]
  print $ queens 6
  -- 10
  print $ length $ queens 5
  -- 4
  print $ length $ queens 6
  -- 40
  print $ length $ queens 7
  -- 92
  print $ length $ queens 8
  -- 352
  print $ length $ queens 9
