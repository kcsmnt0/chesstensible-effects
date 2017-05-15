module Grid.Array where

import Chess
import Data.Array as Array
import Grid

instance Grid a (Array Index a) where
  empty (w,h) = listArray ((0,0), (w-1,h-1)) . repeat
  size b = let (w,h) = snd (bounds b) in (w+1,h+1)
  (!) = (Array.!)
  replace i x = (// [(i,x)])

type ArrayBoard = Array Index (Maybe Piece)
