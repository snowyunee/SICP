
squareLlist []     = []

squareLlist (x:xs) = (x*x):(squareLlist xs)


squareLlist' = map (\x -> x*x) 

main = do
  print $ squareLlist [1,2,3,4]
  print $ squareLlist' [1,2,3,4]
