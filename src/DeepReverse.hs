{-# LANGUAGE MultiParamTypeClasses, OverlappingInstances, FlexibleInstances #-}

module DeepReverse where

class DeepReverse a where
  deepReverse :: [a] -> [a]

instance DeepReverse a => DeepReverse [a] where 
  deepReverse = reverse . map deepReverse

instance DeepReverse a where
  deepReverse = reverse

