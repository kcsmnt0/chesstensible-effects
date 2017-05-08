{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FunctionalDependencies #-}

module Board where

import Chess
import Grid

data Move = Move Index Delta

data Cell = Cell { color :: Color, piece :: Piece }
data Status = Ongoing | Won Color | Tie

class Grid a g => Board a g | g -> a where
  status :: g -> Status
  move :: Move -> g -> g
  moves :: Color -> g -> [Move]
