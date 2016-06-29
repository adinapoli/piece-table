{-- | Implementation of the `PieceTable` data structure
following as closely as possible this paper:

https://www.cs.unm.edu/~crowley/papers/sds.pdf

-}

module Data.Text.PieceTable where

import qualified Data.Set as Set

--------------------------------------------------------------------------------
data Sequence a = Sequence {
    sequence :: Set.Set a
  , position :: !Int
  }

-- | The API

--------------------------------------------------------------------------------
empty :: Ord a => Sequence a
empty = Sequence Set.empty 0

--------------------------------------------------------------------------------
insert :: Ord a => a -> Int -> Sequence a -> Sequence a
insert _ _ s = s
