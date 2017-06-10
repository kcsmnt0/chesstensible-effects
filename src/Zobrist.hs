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
data Key k = Key { key :: k, hash :: Hash } deriving (Show, Eq, Ord)
type Table k = Map (Key k) Rank
data Zobrist k = Zobrist { hashCache :: HashCache , size :: Int , table :: Table k }

initialHashCache :: Member Rand effs => Eff effs HashCache
initialHashCache = fmap Map.fromList $ runChoices $ do
  p <- choose allPieces
  x <- choose [0..4]
  y <- choose [0..5]
  r <- rand
  return (((x,y), p), r)

initialZobrist :: Member Rand effs => Int -> Eff effs (Zobrist k)
initialZobrist size = do
  hc <- initialHashCache
  return $ Zobrist hc size Map.empty

hashBoard :: Board b => HashCache -> b -> Hash
hashBoard hc b = foldr xor 0 [hc Map.! (i,p) | (i, Just p) <- assocs b]

lookup :: Ord k => Key k -> Zobrist k -> Maybe Rank
lookup k = Map.lookup k . table

insert :: Ord k => Key k -> Rank -> Zobrist k -> Zobrist k
insert k r z@Zobrist{..} = case Map.lookup k table of
  Nothing -> Zobrist hashCache size $ Map.insert k r $
    if Map.size table >= size then
      -- since the hashes are random, deleting the key with the minimum hash should be the same as deleting a random key
      -- todo: ...right?
      Map.delete (minimumBy (compare `on` hash) (Map.keys table)) table
    else
      table

  Just r'
    | r == r' -> z
    | otherwise -> error "hash collision"
