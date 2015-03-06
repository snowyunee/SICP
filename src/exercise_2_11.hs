
add_interval x y =
  make_interval ((lower_bound x) + (lower_bound y)) ( (upper_bound x) + (upper_bound y))

-- mul_interval x y =
--   make_interval (min (min p1 p2) (min p3 p4))
--                 (max (max p1 p2) (max p3 p4))
--   where
--     p1 = (lower_bound x) * (lower_bound y)
--     p2 = (lower_bound x) * (upper_bound y)
--     p3 = (upper_bound x) * (lower_bound y)
--     p4 = (upper_bound x) * (upper_bound y)

mul_interval x y 
  | lx < 0 && ux >= 0 && ly < 0 && uy < 0 = make_interval (ux * ly) (lx * uy) -- - + - - 
  | lx < 0 && ux >= 0 && ly < 0 && uy >= 0 = make_interval (min (lx * uy) (ux * ly) ) (max (lx * ly) (ux * uy)) -- - + - +
  | lx < 0 && ux >= 0 && ly >= 0 && uy >= 0 = make_interval (lx * uy) (ux * uy) -- - + + +
  | lx >= 0 && ux >= 0 && ly < 0 && uy < 0 = make_interval (ux * ly) (lx * uy) -- + + - -
  | lx >= 0 && ux >= 0 && ly < 0 && uy >= 0 = make_interval (ux * ly) (ux * uy) -- + + - +
  | lx >= 0 && ux >= 0 && ly >= 0 && uy >= 0 = make_interval (lx * ly) (ux * uy) -- + + + +
  | lx < 0 && ux < 0 && ly < 0 && uy < 0 = make_interval (ux * uy) (lx * ly) -- - - - -
  | lx < 0 && ux < 0 && ly < 0 && uy >= 0 = make_interval (lx * uy) (lx * ly) -- - - - +
  | otherwise = make_interval (lx * uy) (ux * ly) --lx < 0 && ux < 0 && ly >= 0 && uy >= 0 = make_interval (lx * uy) (ux * ly) -- - - + +
  where
    lx = lower_bound x
    ux = upper_bound x
    ly = lower_bound y
    uy = upper_bound y



width x = ((upper_bound x) - (lower_bound x)) / 2

div_interval x y
  | (lower_bound y) == 0 = error "Division by zero"
  | (upper_bound y) == 0 = error "Division by zero"
  | width y == 0 = error "Division by zero"
  | otherwise = mul_interval x (make_interval (1.0 / (upper_bound y))
                                              (1.0 / (lower_bound y)))

sub_interval x y = 
  make_interval ((lower_bound x) - (lower_bound y)) ( (upper_bound x) - (upper_bound y))

make_interval a b = (a, b)

upper_bound x = max (fst x) (snd x)

lower_bound x = min (fst x) (snd x)

make_center_width c w = make_interval (c-w) (c+w)

center x = ((lower_bound x) + (upper_bound x)) / 2

approximate_mul_interval x y =
  make_center_percent ((center x) * (center y))  ((percent x) + (percent y)) 




main :: IO ()
main = do
  let a = make_interval (-10) (-20)
      b = make_interval (0) (-1)
      c = make_interval (-1) (-1)
      s_a = make_interval (10) (11)
      s_b = make_interval (100) (101)
  print $ "mul_interval"
  print $ mul_interval a b
  print $ "approximate_mul_interval"
  print $ approximate_mul_interval a b
  print "a"
  print a
  print "b"
  print b
  print "c"
  print c
  print "mul a b"
  print $ mul_interval a b
  print "mul a c"
  print $ mul_interval a c
  print $ "mul_interval"
  print $ mul_interval s_a s_b
  print $ "approximate_mul_interval"
  print $ approximate_mul_interval s_a s_b

