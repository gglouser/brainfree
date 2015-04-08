{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}
{-|
A free monad representing an abstract brainfuck machine.
-}
module BrainFree (
  -- * BF monad
  BF, BFF(..)
  , movePtr, readPtr, writePtr, getChr, putChr, loop
  , runBF, runBFM
  ) where

import Control.Monad.Free.Church
import Runtime (Cell, Offset)

-- | Alias for the free monad on 'BFF' actions.
type BF = F BFF

-- | A functor representing abstract brainfuck machine actions.
data BFF k
    = MovePtr  !Offset          k       -- ^ Move the data pointer.
    | ReadPtr  !Offset (Cell -> k)      -- ^ Read the cell at offset from data pointer.
    | WritePtr !Offset !Cell    k       -- ^ Write to cell at offset from data pointer.
    | GetChar  (Maybe Char   -> k)      -- ^ Get input. 'Nothing' indicates end-of-file.
    | PutChar  !Char            k       -- ^ Send output.
    | Loop     !Offset (BF ())  k       -- ^ Execute a loop, using cell at offset for condition.
    deriving Functor

movePtr :: MonadFree BFF m => Offset -> m ()
movePtr off = liftF $ MovePtr off ()

readPtr :: MonadFree BFF m => Offset -> m Cell
readPtr off = liftF $ ReadPtr off id

writePtr :: MonadFree BFF m => Offset -> Cell -> m ()
writePtr off n = liftF $ WritePtr off n ()

getChr :: MonadFree BFF m => m (Maybe Char)
getChr = liftF $ GetChar id

putChr :: MonadFree BFF m => Char -> m ()
putChr c = liftF $ PutChar c ()

loop :: MonadFree BFF m => Offset -> BF () -> m ()
loop off body = liftF $ Loop off body ()

-- | Tear down a 'BF' program using the given function to handle 'BFF' actions
-- ('iter' specialized to 'BFF').
runBF :: (BFF a -> a) -> BF a -> a
runBF f p = runF p id f

-- | Tear down a 'BF' program in a 'Monad' using the given function to handle
-- 'BFF' actions ('iterM' specialized to 'BFF').
runBFM :: Monad m => (BFF (m a) -> m a) -> BF a -> m a
runBFM = iterM
