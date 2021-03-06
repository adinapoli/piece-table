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


NOTE0: Currently, only UTF-8 is supported.
NOTE1: At this point, I'm perfectly aware performance is gonna be awful.
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
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text.IO as T
import qualified System.IO.MMap as MMap
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Data.Foldable
import           Debug.Trace
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import           Foreign.ForeignPtr
import           Data.UUID.V4 as UUID
import           Data.UUID as UUID
import           Data.Word
import           Control.Exception (bracket)
import           Control.Monad.ST.Strict
import           System.IO.Unsafe
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
          startPtr = plusPtr fp_ptr start
      in case fileType of
        Original -> acc <> unsafePerformIO (B.unsafePackCStringFinalizer startPtr len (return ()))
        Buffer   ->
          let (fptr, rawLen) = VS.unsafeToForeignPtr0 (VS.slice start len addBufferVec)
          in acc <> unsafePerformIO (withForeignPtr fptr $ \ptr ->
                --B.unsafePackCStringFinalizer
                B.unsafePackCStringFinalizer (castPtr ptr) rawLen (return ())
                )
                -- BS.packCStringLen (castPtr ptr, len * sizeOf (undefined :: Int))
            --acc <> B.take len (B.drop start addBuffer)

{- | We want to support the API as described in the paper:

typedef Position = int; //a position in the sequence, starts from 0
typedef Item unsigned char;

-}

type Position = Int
type Item = B.ByteString


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
      table       = Seq.singleton (Piece Original offset size)
    , tableLength = 1
    , tableSize   = 256
    , fileBuffer  = FileBuffer fp fbPtr rawSize offset size
    , addBuffer   = AddBuffer (VS.replicate 256 (0 :: Word8)) 0 256
    }

--------------------------------------------------------------------------------
newFromText' :: Maybe ViewPort -> T.Text -> IO PieceTable
newFromText' mbViewPort txt = do
  let range = (\ViewPort{..} -> (view_offset, view_size)) <$> mbViewPort
  tmpDir    <- getTemporaryDirectory
  uuid      <- UUID.toString <$> UUID.nextRandom
  bracket (openTempFile tmpDir (uuid <> ".bin")) (hClose . snd) $ \(fp, hdl) -> do
    hSetBuffering hdl NoBuffering
    let txtBytes = TE.encodeUtf8 txt
    let txtBytesLen = B.length txtBytes
    _ <- B.hPutStr hdl txtBytes
    hClose hdl `seq` return ()

    (fbPtr, rawSize, offset, size) <- MMap.mmapFilePtr fp MMap.ReadOnly range
    return $! PieceTable {
        table       = Seq.singleton (Piece Original offset size)
      , tableLength = 1
      , tableSize   = 256
      , fileBuffer  = FileBuffer fp fbPtr rawSize offset size
      , addBuffer   = AddBuffer {
            addBufferVec = VS.replicate 256 (0 :: Word8)
          , addBufferLength = 0
          , addBufferSize = 256
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
insert txt p t@PieceTable{..} =
  let (l, r) = findInsertionPoint p 0 (mempty, table)
      newL = tableLength + 1
      AddBuffer{..} = addBuffer
 in PieceTable {
   table       = (l Seq.|> Piece Buffer addBufferLength (B.length txt)) Seq.>< r
 , tableLength = newL
 , tableSize   = if newL > tableSize then tableSize * 2 else tableSize
 , fileBuffer  = fileBuffer
 , addBuffer   =
   let newABLen = addBufferLength + B.length txt
       newBSize = if newABLen > addBufferSize then addBufferSize * 2 else addBufferSize
   in AddBuffer {
       addBufferVec    = VS.modify (newAtEnd txt addBufferLength newABLen addBufferSize) addBufferVec
     , addBufferLength = newABLen
     , addBufferSize   = newBSize
     }
 }
  where
    ----------------------------------------------------------------------------
    newAtEnd :: forall s. B.ByteString -> Int -> Int -> Int -> VSM.MVector s Word8 -> ST s ()
    newAtEnd el oldL newL currentSize oldVect = do
      targetVect <- if (newL > currentSize) then VSM.grow oldVect (tableSize * 2) else return oldVect
      -- TODO: Slow and ugly!
      forM_ (zip [0 ..] (B.unpack el)) (\(idx, e) -> VSM.write targetVect (oldL + idx) e)

    ----------------------------------------------------------------------------
    findInsertionPoint :: Int -> Int -> (Seq Piece, Seq Piece) -> (Seq Piece, Seq Piece)
    findInsertionPoint targetPos posCount (lq, rq) = case Seq.viewl rq of
      Seq.EmptyL  -> (lq,rq)
      e Seq.:< xs -> case targetPos of
        0 -> (mempty, lq Seq.>< rq)
        _ -> if (posCount + len e) > targetPos then
          -- If we found an insertion point we need to split the piece in 2.
          let p1 = Piece (fileType e) (start e)       (targetPos - posCount)
              p2 = Piece (fileType e) targetPos (len e - targetPos)
          in (lq Seq.|> p1, p2 Seq.<| xs)
          else findInsertionPoint targetPos (posCount + len e) (lq Seq.|> e, xs)

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
