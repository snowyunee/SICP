
add_interval x y =
  make_interval ((lower_bound x) + (lower_bound y)) ( (upper_bound x) + (upper_bound y))

mul_interval x y =
  make_interval (min (min p1 p2) (min p3 p4))
                (max (max p1 p2) (max p3 p4))
  where
    p1 = (lower_bound x) * (lower_bound y)
    p2 = (lower_bound x) * (upper_bound y)
    p3 = (upper_bound x) * (lower_bound y)
    p4 = (upper_bound x) * (upper_bound y)

width x = abs ((upper_bound x) - (lower_bound x))

zero_span x 
  | lx == ux = true
  | otherwise = false
  where
    lx = lower_bound x
    ux = upper_bound x


div_interval x y
  | zero_span y == 0 = error "Division by zero"
  | otherwise = mul_interval x (make_interval (1.0 / (upper_bound y))
                                              (1.0 / (lower_bound y)))

sub_interval x y = 
  make_interval ((lower_bound x) - (lower_bound y)) ( (upper_bound x) - (upper_bound y))

make_interval a b = (a, b)

upper_bound x = max (fst x) (snd x)

lower_bound x = min (fst x) (snd x)

main :: IO ()
main = do
  let a = make_interval (-10) (-20)
      b = make_interval (0) (-1)
      c = make_interval (-1) (-1)
      d = make_interval (1) (2)
  print "a"
  print a
  print "b"
  print b
  print "c"
  print c
  print "d"
  print d
  print "------"
  -- print "div a b"
  -- print $ div_interval a b
  -- print "div a c"
  -- print $ div_interval a c
  print "div a d"
  print $ div_interval a d

