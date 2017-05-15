module Grid.List where

import Grid

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = let (ls, _:rs) = splitAt i xs in ls ++ x : rs

-- warning: very slow
instance Grid a [[a]] where
  empty (w,h) = replicate w . replicate h
  size xss = (length xss, length (head xss))
  g!(x,y) = g !! x !! y
  replace (x,y) a g = replaceAt x (replaceAt y a (g !! x)) g
