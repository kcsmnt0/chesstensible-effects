module Zobrist where

import Chess
import Control.Monad.Freer
import Control.Monad.Freer.Choice
import Control.Monad.Freer.Rand
import Data.Bits
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Word
import Grid

type HashKey = (Index, Piece)
type Hash = Word64
type HashCache = Map HashKey Hash

-- todo: this could just be a generic Key a = Key a Hash
data Key = Key
  { depth :: Int
  , turnsLeft :: Int
  , evaluatingPlayer :: Player
  , alpha :: Rank
  , beta :: Rank
  , boardHash :: Hash
  } deriving (Show, Eq, Ord)

type Table = Map Key Rank

data Zobrist = Zobrist
  { hashCache :: HashCache
  , size :: Int
  , table :: Table
  }

initialHashCache :: Member Rand effs => Eff effs HashCache
initialHashCache = fmap Map.fromList $ runChoices $ do
  p <- choose allPieces
  x <- choose [0..4]
  y <- choose [0..5]
  r <- rand
  return (((x,y), p), r)

hash :: Board b => HashCache -> b -> Hash
hash hc b = foldr xor 0 [hc Map.! (i,p) | (i, Just p) <- assocs b]

key :: Board b => Int -> Int -> Player -> Rank -> Rank -> b -> HashCache -> Key
key d t p al bt bd hc = Key d t p al bt (hash hc bd)

lookup :: Key -> Zobrist -> Maybe Rank
lookup k = Map.lookup k . table

insertKey :: Key -> Rank -> Zobrist -> Zobrist
insertKey k r z@Zobrist{..} = case Map.lookup k table of
  Nothing -> Zobrist hashCache size $ Map.insert k r $
    if Map.size table >= size then
      -- since the hashes are random, deleting the key with the minimum hash should be the same as deleting a random key
      -- todo: ...right?
      Map.delete (minimumBy (compare `on` boardHash) (Map.keys table)) table
    else
      table

  Just r'
    | r == r' -> z
    | otherwise -> error "hash collision"

insert :: Board b => Int -> Int -> Player -> Rank -> Rank -> b -> Rank -> Zobrist -> Zobrist
insert d t p al bt bd r z = insertKey (key d t p al bt bd (hashCache z)) r z
