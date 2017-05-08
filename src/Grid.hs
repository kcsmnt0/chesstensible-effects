{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, RankNTypes #-}

module Grid where

type Bounds = (Int,Int)
type Index = (Int,Int) -- increasing right and down

class Grid a g | g -> a where -- there might be no point in parameterizing this over a
  empty :: Bounds -> a -> g
  size :: g -> Bounds
  (!) :: g -> Index -> a
  replace :: Index -> a -> g -> g

inBounds :: Bounds -> Index -> Bool
inBounds (w,h) (x,y) = 0 <= x && x < w && 0 <= y && y < h

within :: Grid a g => Index -> g -> Bool
i `within` g = inBounds (size g) i

indices :: Grid a g => g -> [Index]
indices g = let (w,h) = size g in [(x,y) | x <- [0..w], y <- [0..h]]

elems :: Grid a g => g -> [a]
elems g = map (g!) $ indices g

entries :: Grid a g => g -> [(Index,a)]
entries g = map (\i -> (i, g!i)) $ indices g
