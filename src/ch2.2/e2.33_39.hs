import Control.Monad
import Tree


nil = []

cons = (:)

-- foldr
accumulate :: (a -> b -> b) -> b -> [a] -> b
accumulate op initial [] = initial
accumulate op initial (x:xs) = op x (accumulate op initial xs)



mymap :: (a -> b) -> [a] -> [b]
mymap p sequence =
  accumulate (\x initial -> cons (p x) initial) nil sequence


append seq1 seq2 =
  accumulate cons seq2 seq1


mylength :: [a] -> Int
mylength sequence =
  accumulate (\x initial -> initial + 1) 0 sequence


horner_eval x coefficient_sequence =
  accumulate (\this_coeff higher_terms -> higher_terms * x + this_coeff)
             0
             coefficient_sequence

treeToList :: Tree a -> [a]
treeToList (Node x) = [x]
treeToList (SubTree l) = accumulate (\x i -> i ++ treeToList x) [] l


count_leaves t =
  accumulate (+) 0 (mymap (\x -> 1) (treeToList t))



tree = SubTree [Node 1, SubTree [Node 2, SubTree [Node 3, Node 5], Node 5], SubTree [Node 6, Node 7]]

accumulate_n op init l@((x:xs):xss) =
 cons (accumulate op init (map head l)) (accumulate_n op init (map tail l))
accumulate_n op init _ = []



--dot_product v w =
--  accumulate (+) 0 (mymap (*) v w)
--
--matrix_mul_vector m v =
--  map () m
--
--transpose mat
--  accumulate_n - - mat
--
--matrix_mul_matrix m n
--  let cols = transpose n
--  in map - m


main = do
  print $ mymap (* 2) [1,2,3]
  print $ append [1,2,3] [4,5,6]
  print $ mylength [1,2,3,4,5]
  print $ "it should be 79\n"
  print $ horner_eval 2 [1,3,0,5,0,1]
  print $ "7"
  print $ count_leaves tree
  print $ "[22,26,30]"
  print $ accumulate_n (+) 0 [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]
--  print $ dot_product [10,10,10] [[1,1,1],[2,2,2],[3,3,3]]

