{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- warning: very slow
module Grid.List where

import Grid

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = undefined

instance Grid a [[a]] where
  maxIndex xss = (length xss, length (head xss))
  g!(x,y) = g!!x!!y
  replace (x,y) a g = replaceAt x (replaceAt y a (g!!x)) g
