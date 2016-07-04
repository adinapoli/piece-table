{-- | Implementation of the `PieceTable` data structure
following as closely as possible this paper:

https://www.cs.unm.edu/~crowley/papers/sds.pdf

Trying to implement it using RB trees or equally performing
data structures as described:

http://e98cuenc.free.fr/wordprocessor/piecetable.html


NOTE: At this point, I'm perfectly aware performance is gonna be awful.
Not worrying about that now.

-}

module Data.Text.PieceTable where

--------------------------------------------------------------------------------
import qualified Data.Text as T
import qualified Data.Text.IO as T

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
  , start  :: !Int
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
new :: FilePath -> IO PieceTable
new fp = do
  c <- T.readFile fp
  return $ PieceTable {
    table = [Piece Original 0 (T.length c)]
    , fileBuffer = c
    , addBuffer  = T.empty
    }
