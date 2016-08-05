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
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.IO as T
import qualified System.IO.MMap as MMap
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Data.Foldable
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import           Foreign.ForeignPtr.Safe
import           Foreign.Storable
import           Data.UUID.V4 as UUID
import           Data.UUID as UUID
import           Data.Word
import           Control.Monad.ST.Strict
import           Debug.Trace
import           Control.Monad
import           System.IO.Unsafe
import           Control.Concurrent (myThreadId)
import           System.Directory
import           System.IO (openTempFile, hClose, hSetBuffering, BufferMode(..))
import           Data.Monoid
import           Foreign.Ptr
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
data AddBuffer a = AddBuffer {
    addBufferVec    :: VS.Vector a
  , addBufferLength :: !Int
  , addBufferSize   :: !Int
  }

--------------------------------------------------------------------------------
data PieceTable = PieceTable {
    table       :: Seq Piece
  , tableLength :: !Int
  , tableSize   :: !Int
  , fileBuffer  :: FileBuffer Word8
  , addBuffer   :: AddBuffer  Word8
  }

-- Idea: Try to use a Storable Vector and later on
-- mapping only a piece of the string using:
-- http://stackoverflow.com/questions/22508826/converting-vector-to-bytestring-in-haskell

--------------------------------------------------------------------------------
-- | Renders the **entire** `PieceTable` to a `Text`. Extremely useful for
-- debugging and testing, but unsafe in the sense it will load the entire content
-- into memory.
unsafeRender :: PieceTable -> T.Text
unsafeRender PieceTable{..} = TE.decodeUtf8 $ foldl' mapPiece mempty table
  where
    mapPiece :: B.ByteString -> Piece -> B.ByteString
    mapPiece !acc Piece{..} =
      let FileBuffer{..}  = fileBuffer
          AddBuffer{..}   = addBuffer
          startPtr = plusPtr fp_ptr (fp_offset + start)
      in case fileType of
        Original -> acc <> unsafePerformIO (B.unsafePackCStringFinalizer startPtr fp_size (return ()))
        Buffer   ->
          let (fptr, rawLen) = VS.unsafeToForeignPtr0 (VS.slice start len addBufferVec)
          in acc <> unsafePerformIO (withForeignPtr fptr $ \ptr ->
                --B.unsafePackCStringFinalizer
                B.packCStringLen (castPtr ptr, len * sizeOf (undefined :: Word8))
                )
                -- BS.packCStringLen (castPtr ptr, len * sizeOf (undefined :: Int))
            --acc <> B.take len (B.drop start addBuffer)

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

  return $! PieceTable {
      table       = Seq.singleton (Piece Original 0 size)
    , tableLength = 1
    , tableSize   = 256
    , fileBuffer  = FileBuffer fp  fbPtr rawSize offset size
    , addBuffer   = AddBuffer (VS.replicate 256 (0 :: Word8)) 0 256
    }

--------------------------------------------------------------------------------
newFromText' :: Maybe ViewPort -> T.Text -> IO PieceTable
newFromText' mbViewPort txt = do
  let range = (\ViewPort{..} -> (view_offset, view_size)) <$> mbViewPort
  tmpDir    <- getTemporaryDirectory
  uuid    <- UUID.toString <$> UUID.nextRandom
  (fp, hdl) <- openTempFile tmpDir (uuid <> ".bin")
  hSetBuffering hdl NoBuffering
  _ <- T.hPutStr hdl txt
  hClose hdl `seq` return ()
  (fbPtr, rawSize, offset, size) <- MMap.mmapFilePtr fp MMap.ReadOnly range
  return $! PieceTable {
      table       = Seq.singleton (Piece Original 0 (T.length txt))
    , tableLength = 1
    , tableSize   = 256
    , fileBuffer  = FileBuffer fp fbPtr rawSize offset size
    , addBuffer   = AddBuffer {
        addBufferVec = VS.replicate 256 (0 :: Word8)
        , addBufferSize = 256
        , addBufferLength = 0
        }
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
    , addBuffer  = AddBuffer mempty 0 0
    }

--------------------------------------------------------------------------------
-- | Inserts a single `Item` at `Position` in the given `PieceTable`.
insert :: Item -> Position -> PieceTable -> PieceTable
insert i p t@PieceTable{..} = case findInsertionPoint of
  Nothing ->
    let newL = tableLength + 1
        AddBuffer{..} = addBuffer
    in PieceTable {
      table       = table Seq.|> Piece Buffer addBufferLength 1
    , tableLength = newL
    , tableSize   = if newL > tableSize then tableSize * 2 else tableSize
    , fileBuffer  = fileBuffer
    , addBuffer   =
      let newABLen = addBufferLength + 1
          newBSize = if newABLen > addBufferSize then addBufferSize * 2 else addBufferSize
      in AddBuffer {
          addBufferVec    = VS.modify (newCharAtEnd (fromIntegral . fromEnum $ i) newABLen addBufferSize) addBufferVec
        , addBufferLength = newABLen
        , addBufferSize   = newBSize
        }
    }
  Just _  -> t
  where
    newCharAtEnd :: forall s. Word8 -> Int -> Int -> VSM.MVector s Word8 -> ST s ()
    newCharAtEnd el newL currentSize oldVect = do
      targetVect <- if (newL > currentSize) then VSM.grow oldVect (tableSize * 2) else return oldVect
      VSM.write targetVect (newL - 1) el

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
