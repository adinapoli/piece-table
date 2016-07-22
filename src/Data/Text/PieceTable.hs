{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
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
  -- * Destroy a piece table.
  , destroy
  -- * Unsafe functions
  , unsafeRender
  ) where

--------------------------------------------------------------------------------
import qualified Data.Text as T
import qualified Data.List as List
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.Text.IO as T
import qualified System.IO.MMap as MMap
import           System.IO.Unsafe
import           Control.Concurrent (myThreadId)
import           System.Directory
import           System.IO (openTempFile, hClose, hSetBuffering, BufferMode(..))
import           Data.Monoid
import           Foreign.Ptr
import           Foreign.C.Types

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
  , len :: !Int
    -- ^ The length of the piece.
  }

--------------------------------------------------------------------------------
data PieceTable = PieceTable {
    table :: [Piece]
  , fileBuffer :: FileBuffer CChar
  , addBuffer  :: B.ByteString
  }

--------------------------------------------------------------------------------
-- | Renders the **entire** `PieceTable` to a `Text`. Extremely useful for
-- debugging and testing, but unsafe in the sense it will load the entire content
-- into memory.
unsafeRender :: PieceTable -> T.Text
unsafeRender PieceTable{..} = case table of
  []  -> T.empty
  lst -> TE.decodeUtf8 $ List.foldl' mapPiece mempty lst
  where
    mapPiece :: B.ByteString -> Piece -> B.ByteString
    mapPiece !acc Piece{..} =
      let FileBuffer{..}  = fileBuffer
          startPtr = plusPtr fp_ptr (fp_offset + start)
      in case fileType of
        Original -> acc <> unsafePerformIO (B.packCStringLen (fp_ptr, fp_size))
        Buffer   -> acc <> B.take len (B.drop start addBuffer)

{- | We want to support the API as described in the paper:

typedef Position = int; //a position in the sequence, starts from 0
typedef Item unsigned char;

-}

type Position = Int
type Item = Char


--------------------------------------------------------------------------------
data FileBuffer a = FileBuffer {
    fb_path :: FilePath
  , fp_ptr  :: Ptr a
  , fp_rawSize :: !Int
  , fp_offset  :: !Int
  , fp_size    :: !Int
  }

--------------------------------------------------------------------------------
new :: FilePath -> IO PieceTable
new fp = do
  (fbPtr, rawSize, offset, size) <- MMap.mmapFilePtr fp MMap.ReadOnly Nothing
  return $! PieceTable {
    table = [Piece Original 0 size]
    , fileBuffer = FileBuffer fp fbPtr rawSize offset size
    , addBuffer  = mempty
    }

--------------------------------------------------------------------------------
newFromText :: T.Text -> IO PieceTable
newFromText txt = do
  tmpDir    <- getTemporaryDirectory
  tid       <- show <$> myThreadId
  (fp, hdl) <- openTempFile tmpDir (tid <> ".bin")
  hSetBuffering hdl NoBuffering
  _ <- T.hPutStr hdl txt
  hClose hdl `seq` return ()
  (fbPtr, rawSize, offset, size) <- MMap.mmapFilePtr fp MMap.ReadOnly Nothing
  return $! PieceTable {
    table = [Piece Original 0 (T.length txt)]
    , fileBuffer = FileBuffer fp fbPtr rawSize offset size
    , addBuffer  = mempty
    }

--------------------------------------------------------------------------------
empty :: PieceTable
empty = PieceTable {
    table = mempty
    , fileBuffer = FileBuffer mempty nullPtr 0 0 0
    , addBuffer  = mempty
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
-- | "Destroys" a `PieceTable`, by unloading the underlying content from memory.
-- Any other operation on the `PieceTable` will yield undefined behaviour.
destroy :: PieceTable -> IO ()
destroy PieceTable {fileBuffer} = MMap.munmapFilePtr (fp_ptr fileBuffer) (fp_rawSize fileBuffer)

--------------------------------------------------------------------------------
replace :: Position
        -- ^ start
        -> T.Text
        -> PieceTable
        -> PieceTable
replace _ _ p = p
