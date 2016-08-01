{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
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
  , ViewPort
  , newViewPort
  , viewPortOffset
  , viewPortSize
  , Piece
  , PieceTable
  , new
  , new'
  , newFromText
  , newFromText'
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
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.IO as T
import qualified System.IO.MMap as MMap
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import           Control.Monad.ST.Strict
import           Debug.Trace
import           Control.Monad
import           System.IO.Unsafe
import           Control.Concurrent (myThreadId)
import           System.Directory
import           System.IO (openTempFile, hClose, hSetBuffering, BufferMode(..))
import           Data.Monoid
import           Foreign.Ptr
import           Foreign.C.Types
import           Data.Int

--------------------------------------------------------------------------------
data FileType = Original
              | Buffer
              deriving Show

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
  } deriving Show


--------------------------------------------------------------------------------
data PieceTable = PieceTable {
    table       :: V.Vector Piece
  , tableLength :: !Int
  , tableSize   :: !Int
  , fileBuffer  :: FileBuffer CChar
  , addBuffer   :: B.ByteString
  }

--------------------------------------------------------------------------------
-- | Renders the **entire** `PieceTable` to a `Text`. Extremely useful for
-- debugging and testing, but unsafe in the sense it will load the entire content
-- into memory.
unsafeRender :: PieceTable -> T.Text
unsafeRender PieceTable{..} = let t = V.toList . V.take tableLength $ table in case (traceShowId t) of
  []  -> T.empty
  lst -> TE.decodeUtf8 $ List.foldl' mapPiece mempty lst
  where
    mapPiece :: B.ByteString -> Piece -> B.ByteString
    mapPiece !acc Piece{..} =
      let FileBuffer{..}  = fileBuffer
          startPtr = plusPtr fp_ptr (fp_offset + start)
      in case fileType of
        Original -> acc <> unsafePerformIO (B.packCStringLen (startPtr, fp_size))
        Buffer   -> acc <> B.take len (B.drop start addBuffer)

{- | We want to support the API as described in the paper:

typedef Position = int; //a position in the sequence, starts from 0
typedef Item unsigned char;

-}

type Position = Int
type Item = Char


--------------------------------------------------------------------------------
data FileBuffer a = FileBuffer {
    fb_path    :: FilePath
  , fp_ptr     :: Ptr a
  , fp_rawSize :: !Int
  , fp_offset  :: !Int
  , fp_size    :: !Int
  }


--------------------------------------------------------------------------------
data ViewPort = ViewPort {
    view_offset :: !Int64
  , view_size   :: !Int
  }

--------------------------------------------------------------------------------
newViewPort :: Int64 -> Int -> ViewPort
newViewPort = ViewPort

--------------------------------------------------------------------------------
viewPortOffset :: ViewPort -> Int64
viewPortOffset = view_offset

--------------------------------------------------------------------------------
viewPortSize :: ViewPort -> Int
viewPortSize = view_size

--------------------------------------------------------------------------------
new :: FilePath -> IO PieceTable
new = new' Nothing

--------------------------------------------------------------------------------
new' :: Maybe ViewPort -> FilePath -> IO PieceTable
new' mbViewPort fp = do
  let range = (\ViewPort{..} -> (view_offset, view_size)) <$> mbViewPort
  (fbPtr, rawSize, offset, size) <- MMap.mmapFilePtr fp MMap.ReadOnly range
  vect <- VM.new 256
  VM.write vect 0 (Piece Original 0 size)
  t <- V.freeze vect
  return $! PieceTable {
      table       = t
    , tableLength = 1
    , tableSize   = 256
    , fileBuffer  = FileBuffer fp fbPtr rawSize offset size
    , addBuffer   = mempty
    }

--------------------------------------------------------------------------------
newFromText' :: Maybe ViewPort -> T.Text -> IO PieceTable
newFromText' mbViewPort txt = do
  let range = (\ViewPort{..} -> (view_offset, view_size)) <$> mbViewPort
  tmpDir    <- getTemporaryDirectory
  tid       <- show <$> myThreadId
  (fp, hdl) <- openTempFile tmpDir (tid <> ".bin")
  hSetBuffering hdl NoBuffering
  _ <- T.hPutStr hdl txt
  hClose hdl `seq` return ()
  (fbPtr, rawSize, offset, size) <- MMap.mmapFilePtr fp MMap.ReadOnly range
  vect <- VM.new 256
  VM.write vect 0 (Piece Original 0 (T.length txt))
  t <- V.freeze vect
  return $! PieceTable {
      table       = t
    , tableLength = 1
    , tableSize   = 256
    , fileBuffer  = FileBuffer fp fbPtr rawSize offset size
    , addBuffer   = mempty
    }

--------------------------------------------------------------------------------
newFromText :: T.Text -> IO PieceTable
newFromText = newFromText' Nothing

--------------------------------------------------------------------------------
empty :: PieceTable
empty = PieceTable {
      table = mempty
    , tableSize = 0
    , tableLength = 0
    , fileBuffer = FileBuffer mempty nullPtr 0 0 0
    , addBuffer  = mempty
    }

--------------------------------------------------------------------------------
-- | Inserts a single `Item` at `Position` in the given `PieceTable`.
insert :: Item -> Position -> PieceTable -> PieceTable
insert i p t@PieceTable{..} = case findInsertionPoint of
  Nothing -> let newL = tableLength + 1 in PieceTable {
      table       = V.modify (newPieceAtEnd newL) table
    , tableLength = newL
    , tableSize   = if newL > tableSize then tableSize * 2 else tableSize
    , fileBuffer  = fileBuffer
    , addBuffer   = addBuffer <> C8.singleton i
    }
  Just x  -> let piece = table V.! x in case fileType piece of
    Original -> t
    Buffer   -> t
  where
    newPieceAtEnd :: forall s. Int -> VM.MVector s Piece -> ST s ()
    newPieceAtEnd newL oldVect = do
      targetVect <- if (newL > tableSize) then VM.grow oldVect (tableSize * 2) else return oldVect
      VM.write targetVect (newL - 1) (Piece Buffer (B.length addBuffer) 1)

    findInsertionPoint :: Maybe Int
    findInsertionPoint = Nothing

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
