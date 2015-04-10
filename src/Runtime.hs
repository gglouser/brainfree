{-|
Common types and utilities for brainfuck programs, including a few
different implementations for the brainfuck data array.
-}
module Runtime where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isAscii, showLitChar)
import qualified Data.Vector.Unboxed.Mutable as V
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Array (advancePtr, withArray)
import Foreign.Storable (peekElemOff, pokeElemOff)
import System.IO (isEOF)

-- | This type is used as the unit of memory by all of our bf machines.
type Cell = Word8

type Offset = Int

-- | Convert between 'Enum' types.
-- This is a convenient way to convert 'Cell' to 'Char' and vice versa,
-- since both are Enums.
coerce :: (Enum a, Enum b) => a -> b
coerce = toEnum . fromEnum

-- | General monadic bf input, given an action to fetch a character
-- and an action to write a cell to the data array.
-- The default behavior is to leave the current cell value unchagned.
bfInputM :: Monad m => m (Maybe Char) -> (Cell -> m ()) -> m ()
bfInputM gc wr = gc >>= maybe (return ()) (wr . coerce)

-- | General monadic bf output, given an action to read a cell
-- and an action to send a character.
bfOutputM :: Monad m => m Cell -> (Char -> m ()) -> m ()
bfOutputM rd pc = rd >>= pc . coerce

-- | General monadic bf loop, given an action that reads the current cell value
-- and an action for the loop body.
bfLoopM :: Monad m => m Cell -> m () -> m ()
bfLoopM rd body = loop'
    where loop' = do x <- rd
                     when (x /= 0) $ body >> loop'

-- | Accept an input character from stdin, returning Nothing on end-of-file.
getc :: MonadIO m => m (Maybe Char)
getc = do eof <- liftIO isEOF
          if eof then return Nothing else liftIO $ fmap Just getChar

-- | Send a character to stdout.
putc :: MonadIO m => Char -> m ()
putc c = liftIO $ if isAscii c then putChar c else putStr $ showLitChar c ""


-- * Infinite tape memory

-- | An infinite stream of 'Cell's
data Stream = Cell :> Stream

-- | A bidirectionally infinite tape with one 'Cell' that can be read
-- from or written to by a notional /tape head/ that can be moved left
-- or right along the tape. Also tracks the current position relative
-- to the /data pointer/.
data Tape = T !Offset Stream !Cell Stream

-- | A tape initialized to all zeros.
blankTape :: Tape
blankTape = T 0 zs 0 zs
    where zs = 0 :> zs

-- | Move tape head one cell to the left.
tapeLeft :: Tape -> Tape
tapeLeft (T p (h' :> l) h r) = T (p-1) l h' (h :> r)

-- | Move tape head one cell to the right.
tapeRight :: Tape -> Tape
tapeRight (T p l h (h' :> r)) = T (p+1) (h :> l) h' r

-- | Read at tape head.
tapeRead' :: Tape -> Cell
tapeRead' (T _ _ h _) = h

-- | Write to tape head.
tapeWrite' :: Cell -> Tape -> Tape
tapeWrite' x (T p l _ r) = T p l x r

-- | Move the data pointer.
-- No need to actually move the tape, just update relative position.
tapeMove :: Offset -> Tape -> Tape
tapeMove n (T p l h r) = T (p - n) l h r

-- | Move tape to offset relative to pointer.
-- Positive offsets are to the right, negative to the left.
tapeMoveToOffset :: Offset -> Tape -> Tape
tapeMoveToOffset off t@(T p _ _ _)
    | off < p   = iterate tapeLeft t !! (p - off)
    | otherwise = iterate tapeRight t !! (off - p)

-- | Get the value of the cell at offset.
tapeRead :: Offset -> Tape -> Cell
tapeRead off = tapeRead' . tapeMoveToOffset off

-- | Set the value of the cell at offset.
tapeWrite :: Offset -> Cell -> Tape -> Tape
tapeWrite off x = tapeWrite' x . tapeMoveToOffset off


-- * Vector memory

-- | A VectorMem consists of a mutable 'Vector' of 'Cell's and an
-- index that acts as a pointer to the current cell.
data VectorMem = VM (V.IOVector Cell) !Int

-- | Allocate a new VectorMem with the given size, with all elements
-- initialized to zero and the pointer pointing to the first element,
-- and then run the given action using the new VectorMem.
withVectorMem :: Int -> (VectorMem -> IO a) -> IO a
withVectorMem size f = do v <- V.replicate size 0
                          f (VM v 0)

-- | Move the data pointer.
vecMove :: Int -> VectorMem -> VectorMem
vecMove n (VM v p) = VM v (p + n)

-- | Get the value of cell at offset.
vecRead :: MonadIO m => Offset -> VectorMem -> m Cell
vecRead off (VM v p) = liftIO $ V.unsafeRead v (p + off)

-- | Set the value of the cell at offset.
vecWrite :: MonadIO m => Offset -> Cell -> VectorMem -> m ()
vecWrite off x (VM v p) = liftIO $ V.unsafeWrite v (p + off) x


-- * Foreign pointer/array memory

type FPtrMem = Ptr Cell

-- | Allocate a new 'FPtrMem' with the given size, with all elements
-- initialized to zero and the pointer pointing to the first element,
-- and then run the given action using the new 'FPtrMem'.
withFPtrMem :: Int -> (FPtrMem -> IO a) -> IO a
withFPtrMem size = withArray (replicate size 0)

-- | Move the data pointer.
fptrMove :: Int -> FPtrMem -> FPtrMem
fptrMove = flip advancePtr

-- | Get the value of the cell at offset.
fptrRead :: MonadIO m => Offset -> FPtrMem -> m Cell
fptrRead off ptr = liftIO $ peekElemOff ptr off

-- | Set the value of the cell at offset.
fptrWrite :: MonadIO m => Offset -> Cell -> FPtrMem -> m ()
fptrWrite off x ptr = liftIO $ pokeElemOff ptr off x
