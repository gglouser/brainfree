module Runtime where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Vector.Unboxed.Mutable as V
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Array (advancePtr, withArray)
import Foreign.Storable (peek, poke)
import System.IO (isEOF)

type Cell = Word8

coerce :: (Enum a, Enum b) => a -> b
coerce = toEnum . fromEnum

bfLoopM :: Monad m => m Cell -> m () -> m ()
bfLoopM rd body = loop'
    where loop' = do x <- rd
                     when (x /= 0) $ body >> loop'

getc :: MonadIO m => m (Maybe Char)
getc = do eof <- liftIO isEOF
          if eof then return Nothing else liftIO $ fmap Just getChar

putc :: MonadIO m => Char -> m ()
putc = liftIO . putChar

-----

data Stream = Cell :> Stream
data Tape = T Stream !Cell Stream

blankTape :: Tape
blankTape = T zs 0 zs
    where zs = 0 :> zs

tapeLeft :: Tape -> Tape
tapeLeft (T (h' :> l) h r) = T l h' (h :> r)

tapeRight :: Tape -> Tape
tapeRight (T l h (h' :> r)) = T (h :> l) h' r

tapeMove :: Int -> Tape -> Tape
tapeMove n t
    | n < 0     = iterate tapeLeft t !! (-n)
    | otherwise = iterate tapeRight t !! n

tapeRead :: Tape -> Cell
tapeRead (T _ h _) = h

tapeWrite :: Cell -> Tape -> Tape
tapeWrite x (T l _ r) = T l x r

-----

data VectorMem = VM (V.IOVector Cell) !Int

withVectorMem :: Int -> (VectorMem -> IO a) -> IO a
withVectorMem size f = do v <- V.replicate size 0
                          f (VM v 0)

vecMove :: Int -> VectorMem -> VectorMem
vecMove n (VM v p) = VM v (p + n)

vecRead :: MonadIO m => VectorMem -> m Cell
vecRead (VM v p) = liftIO $ V.unsafeRead v p

vecWrite :: MonadIO m => Cell -> VectorMem -> m ()
vecWrite x (VM v p) = liftIO $ V.unsafeWrite v p x

-----

type FPtrMem = Ptr Cell

withFPtrMem :: Int -> (FPtrMem -> IO a) -> IO a
withFPtrMem size = withArray (replicate size 0)

fptrMove :: Int -> FPtrMem -> FPtrMem
fptrMove = flip advancePtr

fptrRead :: MonadIO m => FPtrMem -> m Cell
fptrRead = liftIO . peek

fptrWrite :: MonadIO m => Cell -> FPtrMem -> m ()
fptrWrite x p = liftIO $ poke p x
