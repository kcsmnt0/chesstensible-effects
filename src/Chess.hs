{-# LANGUAGE DataKinds, GADTs, FlexibleContexts, ConstraintKinds, DeriveFunctor, TypeFamilies, TupleSections #-}

module Chess where

import Control.Monad
import Data.Char
import Data.Maybe
import Grid

data Player = Black | White deriving Eq -- black starts at top
data Silhouette = Pawn | Knight | Bishop | Rook | Queen | King deriving Eq
data Piece = Piece { player :: Player, silhouette :: Silhouette } deriving Eq
type Board = Grid (Maybe Piece)
data Permission = Occupy | Take | Whatever deriving (Show, Eq)
data Result = Capture Silhouette | Migrate deriving (Show, Eq)
data Trajectory = Offset (Int,Int) | Ray (Int,Int) deriving (Show, Eq) -- how to get to some indices that a piece can potentially move to
data PotentialMove = PotentialMove Permission Trajectory deriving (Show, Eq)
data ActualMove = ActualMove Result Index deriving (Show, Eq)

data PlayerSing (p :: Player) where
  WHITE :: PlayerSing White
  BLACK :: PlayerSing Black

type family Opponent (p :: Player) where
  Opponent White = Black
  Opponent Black = White

instance Show Player where
  show White = "W"
  show Black = "B"

instance Show (PlayerSing p) where
  show WHITE = show White
  show BLACK = show Black

instance Show Silhouette where
  show Pawn = "P"
  show Knight = "N"
  show Bishop = "B"
  show Rook = "R"
  show Queen = "Q"
  show King = "K"

instance Show Piece where
  show (Piece White s) = show s
  show (Piece Black s) = map toLower $ show s

-- todo: HEY THIS IS BACKWARDS FROM THE SERVER WAY
showBoard :: Board b => b -> String
showBoard b = unlines $ header ++ [show y ++ "| " ++ concat [maybe "." show (b!(x,y)) | x <- [0..w-1]] | y <- [0..h-1]]
  where
    header = map (("   " ++) . flip map [0..w-1]) [chr . (ord 'a' +), const '_']
    (w,h) = size b

playerSing :: PlayerSing p -> Player
playerSing WHITE = White
playerSing BLACK = Black

opponent :: Player -> Player
opponent Black = White
opponent White = Black

won :: Board b => Player -> b -> Bool
won p = not . any (maybe False ((p ==) . player)) . elems

lost :: Board b => Player -> b -> Bool
lost = won . opponent

shift :: Index -> Index -> Index
shift (x,y) (x',y') = (x+x',y+y')

rotate :: Trajectory -> Trajectory
rotate (Offset (x,y)) = Offset (y,-x)
rotate (Ray (x,y)) = Ray (y,-x)

rotations :: Trajectory -> [Trajectory]
rotations = take 4 . iterate rotate

down :: Trajectory
down = Ray (0,1)

downRight :: Trajectory
downRight = Ray (1,1)

axes :: [Trajectory]
axes = rotations down

diagonals :: [Trajectory]
diagonals = rotations downRight

at :: Permission -> Index -> PotentialMove
at p = PotentialMove p . Offset

mask :: Piece -> [PotentialMove]
mask (Piece Black Pawn) = [Occupy `at` (0, 1), Take `at` (-1, 1), Take `at` (1, 1)]
mask (Piece White Pawn) = [Occupy `at` (0,-1), Take `at` (-1,-1), Take `at` (1,-1)]
mask (Piece _ Knight) = map (Whatever `at`) [(2,1), (2,-1), (-2,1), (-2,-1), (1,2), (2,-1), (-1,2), (-1,-2)]
mask (Piece _ Bishop) = map (Occupy `at`) [(0,1), (0,-1), (1,0), (-1,0)] ++ map (PotentialMove Whatever) diagonals
mask (Piece _ Rook) = map (PotentialMove Whatever) axes
mask (Piece _ Queen) = map (PotentialMove Whatever) (axes ++ diagonals)
mask (Piece _ King) = map (Whatever `at`) [(1,-1), (1,0), (1,1), (0,-1), (0,1), (-1,-1), (-1,0), (-1,1)]

reachable :: Bounds -> Index -> Trajectory -> [Index]
reachable b i (Offset j) = let i' = shift i j in [i' | inBounds b i']
reachable b i (Ray j) = takeWhile (inBounds b) $ tail $ iterate (shift j) i

result :: Player -> Permission -> Maybe Piece -> Maybe Result
result c Occupy Nothing = Just Migrate
result c Take (Just (Piece c' s)) | (c' == opponent c) = Just $ Capture s
result c Whatever Nothing = Just Migrate
result c Whatever (Just (Piece c' s)) | (c' == opponent c) = Just $ Capture s
result _ _ _ = Nothing

moves :: Board b => b -> Piece -> Index -> [ActualMove]
moves b p i = do
  PotentialMove pm t <- mask p
  i' <- reachable (size b) i t
  e <- maybeToList $ result (player p) pm (b!i')
  return $ ActualMove e i'

maybeMove :: Board b => b -> Index -> Index -> Maybe ActualMove
maybeMove b i j = do
  p <- b!i
  let p' = b!j
  -- todo: this is pretty half-assed, it just sees if it can be found in the moves list
  listToMaybe [m | m@(ActualMove e j') <- moves b p i, j == j', case e of Migrate -> isNothing p'; Capture _ -> isJust p']

makeMove :: Board b => Index -> Index -> b -> b
makeMove i j b = replace i Nothing (replace j (b!i) b)

initialBoard :: Board b => b
initialBoard = foldr1 (.) [replace i (Just x) | (i,x) <- positions] (empty (5,6) Nothing)
  where
    positions = backRow Black 0 [0..4] ++ pawns Black 1 ++ backRow White 5 [4,3..0] ++ pawns White 4
    pawns p y = [((x,y), Piece p Pawn) | x <- [0..4]]
    backRow p y xs = [((x,y), Piece p s) | (x,s) <- zip xs [King, Queen, Bishop, Knight, Rook]]
