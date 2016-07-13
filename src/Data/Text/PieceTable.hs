{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

{-- | Implementation of the `PieceTable` data structure
following as closely as possible this paper:

https://www.cs.unm.edu/~crowley/papers/sds.pdf

Trying to implement it using RB trees or equally performing
data structures as described:

http://e98cuenc.free.fr/wordprocessor/piecetable.html


NOTE: At this point, I'm perfectly aware performance is gonna be awful.
Not worrying about that now.

-}

module Data.Text.PieceTable (
    FileType(..)
  , Piece
  , PieceTable
  , new
  , newFromText
  -- * API as described in the paper
  , empty
  , insert
  , delete
  , itemAt
  -- * Operation on ranges
  , insertSequence
  , deleteSequence
  , sequenceAt
  , replace
  -- * Unsafe functions
  , unsafeRender
  ) where

--------------------------------------------------------------------------------
import qualified Data.Text as T
import qualified Data.List as List
import qualified Data.Text.IO as T
import           Data.Monoid

--------------------------------------------------------------------------------
data FileType = Original
              | Buffer

--------------------------------------------------------------------------------
-- | Each piece descriptor points to a span in the
-- file or in the add buffer. Thus a descriptor must
-- contain three piece of information.
data Piece = Piece {
    fileType :: FileType
    -- ^ Which buffer the `Piece` refers to.
  , start  :: !Position
    -- ^ On offset into that buffer.
  , length :: !Int
    -- ^ The length of the piece.
  }

--------------------------------------------------------------------------------
data PieceTable = PieceTable {
    table :: [Piece]
  , fileBuffer :: T.Text
  , addBuffer  :: T.Text
  }

--------------------------------------------------------------------------------
-- | Renders the **entire** `PieceTable` to a `Text`. Extremely useful for
-- debugging and testing, but unsafe in the sense it will load the entire content
-- into memory.
unsafeRender :: PieceTable -> T.Text
unsafeRender PieceTable{..} = case table of
  []  -> T.empty
  lst -> List.foldl' mapPiece T.empty lst
  where
    mapPiece :: T.Text -> Piece -> T.Text
    mapPiece !acc Piece{..} =
      let render = T.take length . T.drop start
      in case fileType of
        Original -> acc <> render fileBuffer
        Buffer   -> acc <> render addBuffer

{- | We want to support the API as described in the paper:

typedef Position = int; //a position in the sequence, starts from 0
typedef Item unsigned char;

-}

type Position = Int
type Item = Char

--------------------------------------------------------------------------------
new :: FilePath -> IO PieceTable
new fp = do
  c <- T.readFile fp
  return $ new' c

--------------------------------------------------------------------------------
new' :: T.Text -> PieceTable
new' c = PieceTable {
    table = [Piece Original 0 (T.length c)]
    , fileBuffer = c
    , addBuffer  = T.empty
    }

--------------------------------------------------------------------------------
newFromText :: T.Text -> PieceTable
newFromText = new'

--------------------------------------------------------------------------------
empty :: PieceTable
empty = PieceTable {
    table = mempty
    , fileBuffer = T.empty
    , addBuffer  = T.empty
    }

--------------------------------------------------------------------------------
-- | Inserts a single `Item` at `Position` in the given `PieceTable`.
insert :: Item -> Position -> PieceTable -> PieceTable
insert _ _ p = p

--------------------------------------------------------------------------------
insertSequence :: T.Text -> Position -> PieceTable -> PieceTable
insertSequence _ _ p = p

--------------------------------------------------------------------------------
-- | Deletes a single `Item` at `Position` in the given `PieceTable`.
delete :: Position -> PieceTable -> PieceTable
delete _ p = p

--------------------------------------------------------------------------------
deleteSequence :: (Position, Position)
               -- ^ (start, end)
               -> PieceTable
               -- ^ The original `PieceTable`
               -> PieceTable
deleteSequence _ p = p

--------------------------------------------------------------------------------
itemAt :: Position -> PieceTable -> Maybe Item
itemAt _ _ = Nothing

--------------------------------------------------------------------------------
sequenceAt :: Position -> PieceTable -> T.Text
sequenceAt _ _ = T.empty

--------------------------------------------------------------------------------
replace :: Position
        -- ^ start
        -> T.Text
        -> PieceTable
        -> PieceTable
replace _ _ p = p
