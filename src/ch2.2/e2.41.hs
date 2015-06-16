
makeTriple n = [(x,y,z)| x <- [1..n], y <- [1..x-1], z <- [1..y-1]]

f n s =
  filter (\(x,y,z) -> s == x + y + z) $ makeTriple n

main = do
  print $ f 12 12
