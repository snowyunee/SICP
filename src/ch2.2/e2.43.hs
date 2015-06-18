import Debug.Trace

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
          concatMap (\newRow  -> 
                        map (\restOfQueens -> traceShow ((k, newRow, restOfQueens)) (adjoinPosition newRow k restOfQueens))
                            (queenCols (k - 1))) $
          (enumerateInterval 1 boardSize)

main = do
  -- "1"
  -- (1,1,[])
  -- [[(1,1)]]
  print $ "1"
  print $ queens 1
  -- "2"
  -- (1,1,[])
  -- (2,1,[(1,1)])
  -- (1,2,[])
  -- (2,1,[(1,2)])
  -- (1,1,[])
  -- (2,2,[(1,1)])
  -- (1,2,[])
  -- (2,2,[(1,2)])
  -- []
  print $ "2"
  print $ queens 2
  -- [[]]
  print $ "3"
  print $ queens 3
