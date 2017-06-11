module Zobrist where

import Chess
import Control.Monad.Freer
import Control.Monad.Freer.Choice
import Control.Monad.Freer.Rand
import Data.Bits
import Data.Function
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Word
import Grid

type HashKey = (Index, Maybe Piece)
type Hash = Word64
type HashCache = Map HashKey Hash
data Key k = Key { key :: k, hash :: Hash } deriving (Show, Eq, Ord)
type Table k = Map (Key k) Rank
data Zobrist k = Zobrist { hashCache :: HashCache , size :: Int , table :: Table k }

initialHashCache :: Member Rand effs => Eff effs HashCache
initialHashCache = fmap Map.fromList $ runChoices $ do
  p <- choose (Nothing : map Just allPieces)
  x <- choose [0..4]
  y <- choose [0..5]
  r <- rand
  return (((x,y), p), r)

initialZobrist :: Member Rand effs => Int -> Eff effs (Zobrist k)
initialZobrist size = do
  hc <- initialHashCache
  return $ Zobrist hc size Map.empty

boardHash :: Board b => HashCache -> b -> Hash
boardHash hc b = foldr xor 0 $ map (hc Map.!) $ assocs b

moveHash :: Board b => HashCache -> b -> Move -> Hash -> Hash
moveHash hc b (i,j) = foldr1 (.) $ map (xor . (hc Map.!)) [(i, b!i), (i, Nothing), (j, b!j), (j, b!i)]

lookup :: Ord k => Key k -> Zobrist k -> Maybe Rank
lookup k z@Zobrist{..} = Map.lookup k table

insertKey :: Ord k => Key k -> Rank -> Zobrist k -> Zobrist k
insertKey k r z@Zobrist{..} =
  case Map.lookup k table of
    Nothing -> Zobrist hashCache size $ Map.insert k r $
      if Map.size table >= size then
        -- since the hashes are random, deleting the key with the minimum hash is the same as deleting a random key
        -- todo: ...right?
        Map.deleteMin table
      else
        table

    Just r'
      | r == r' -> z
      | otherwise -> error "hash collision"

insertBoard :: (Ord k, Board b) => k -> b -> Rank -> Zobrist k -> Zobrist k
insertBoard k b r z@Zobrist{..} = insertKey (Key k (boardHash hashCache b)) r z

insertMove :: (Ord k, Board b) => k -> b -> Hash -> Move -> Rank -> Zobrist k -> Zobrist k
insertMove k b h m r z@Zobrist{..} = insertKey (Key k (moveHash hashCache b m h)) r z
