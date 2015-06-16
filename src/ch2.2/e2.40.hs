uniqPairs n = concatMap (\i -> map (\j -> (i, j)) (enumerateInterval 1 (i - 1))) (enumerateInterval 1 n)

enumerateInterval l h
  | l > h       = []
  | otherwise   = [l..h]
  


isPrime k = null [ x | x <- [2..k - 1], k `mod`x  == 0]

isPrimeSum pr = isPrime ((fst pr) + (snd pr))


makePairSum pr =
  [f,s,sum]
  where 
    f = fst pr
    s = snd pr
    sum = f + s

primeSumPairs n =
  map makePairSum $ filter isPrimeSum $ uniqPairs n

main = do
  print $ primeSumPairs 10

