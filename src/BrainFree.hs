{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}
{-|
A free monad representing an abstract brainfuck machine.
-}
module BrainFree (
  -- * BF monad
  BF, BFF(..)
  , movePtr, readPtr, writePtr, addPtr, getChr, putChr, loop
  , input, output
  , runBF, runBFM
  -- * Parsing
  , parseString, parseFile
  -- * Optimizer
  , optimize
  -- * Utilities
  , dumpBF
  ) where

import Control.Applicative
import Control.Monad.Free
import Control.Monad.Free.Church (fromF)
import Data.Bifunctor
import qualified Text.Parsec as P
import Runtime (Cell, coerce)

-- | Alias for the free monad on 'BFF' actions.
type BF = Free BFF

-- | A functor representing abstract brainfuck machine actions.
data BFF k = MovePtr !Int k                 -- ^ Move the data pointer the specified amount.
           | ReadPtr (Cell -> k)            -- ^ Read the current cell at the data pointer.
           | WritePtr !Cell k               -- ^ Write the given value to the current cell at the data pointer.
           | AddPtr !Cell k                 -- ^ Add the given value to the current cell at the data pointer.
                                            --   (Redundant given 'ReadPtr' / 'WritePtr', but allows more optimizations.)
           | GetChar (Maybe Char -> k)      -- ^ Accept an input character. @Nothing@ indicates end-of-file.
           | PutChar !Char k                -- ^ Send an output character.
           | Loop (BF ()) k                 -- ^ Execute a loop using the given program fragment as the loop body.
    deriving Functor

movePtr :: MonadFree BFF m => Int -> m ()
movePtr n = liftF (MovePtr n ())

readPtr :: MonadFree BFF m => m Cell
readPtr = liftF (ReadPtr id)

writePtr :: MonadFree BFF m => Cell -> m ()
writePtr c = liftF (WritePtr c ())

addPtr :: MonadFree BFF m => Cell -> m ()
addPtr n = liftF (AddPtr n ())

getChr :: MonadFree BFF m => m (Maybe Char)
getChr = liftF (GetChar id)

putChr :: MonadFree BFF m => Char -> m ()
putChr c = liftF (PutChar c ())

loop :: MonadFree BFF m => BF () -> m ()
loop body = liftF (Loop body ())

-- | The standard brainfuck input action ("@,@").
--
-- The default behavior is to leave the current cell value unchagned.
-- A free monad interpreter for 'BFF' actions can implement one of the
-- other standard behaviors by returning (the equivalent of) @Just 0@
-- or @Just (-1)@ instead of @Nothing@ when end-of-file is reached.
input :: MonadFree BFF m => m ()
input = getChr >>= maybe (return ()) (writePtr . coerce)

-- | The standard brainfuck output action ("@.@").
output :: MonadFree BFF m => m ()
output = readPtr >>= putChr . coerce

-- | Tear down a 'BF' program using the given function to handle 'BFF' actions
-- ('iter' specialized to 'BFF').
runBF :: (BFF a -> a) -> BF a -> a
runBF = iter

-- | Tear down a 'BF' program in a 'Monad' using the given function to handle
-- 'BFF' actions ('iterM' specialized to 'BFF').
runBFM :: Monad m => (BFF (m a) -> m a) -> BF a -> m a
runBFM = iterM

-----

parseBF :: String -> String -> Either String (BF ())
parseBF filename = first show . P.parse (parse' <* P.eof) filename

parse' :: P.Parsec String () (BF ())
parse' = fromF . sequence_ <$> many parse1

parse1 :: MonadFree BFF m => P.Parsec String () (m ())
parse1 = '<' ~> movePtr (-1)
     <|> '>' ~> movePtr 1
     <|> '-' ~> addPtr (-1)
     <|> '+' ~> addPtr 1
     <|> ',' ~> input
     <|> '.' ~> output
     <|> loop <$> P.between (P.char '[') (P.char ']') parse'
     <|> return () <$ P.noneOf "]"
    where c ~> i = i <$ P.char c

-- | Parse a string containing a brainfuck program and return either
-- an error message or a @'BF' ()@ representing the program.
parseString :: String -> Either String (BF ())
parseString = parseBF ""

-- | Parse the contents of the named file as a brainfuck program and
-- return either an error message or a @'BF' ()@ representing the program.
parseFile :: FilePath -> IO (Either String (BF ()))
parseFile filename = parseBF filename <$> readFile filename

-----

-- | Optimize a sequence of actions in the 'BF' monad.
--
-- * Combine data pointer moves ("@>@" and "@<@").
-- * Combine data adds/subtracts ("@+@" and "@-@").
-- * Simple loops "@[-]@" and "@[+]@" become @'WritePtr' 0@.
-- * Cache the value of the current cell at the data pointer and use
--   it to eliminate reads and defer writes.
optimize :: BF a -> BF a
optimize = optimize' (Just 0)

-- 'optimize' worker that passes the cached cell value.
optimize' :: Maybe Cell -> Free BFF a -> Free BFF a

-- Combine moves
optimize' cc (Free (MovePtr a (Free (MovePtr b k)))) = optimize' cc (Free (MovePtr (a+b) k))

-- Move 0 is a nop
optimize' cc (Free (MovePtr 0 k)) = optimize' cc k

-- Spill cell cache on move
optimize' cc (Free (MovePtr a k)) = spillCache cc $ Free (MovePtr a (optimize' Nothing k))

-- Eliminate read if we have a cached value
optimize' cc@(Just c) (Free (ReadPtr k)) = optimize' cc (k c)

-- Otherwise, cache the read
optimize' Nothing (Free (ReadPtr k)) = Free (ReadPtr (\c -> optimize' (Just c) (k c)))

-- Write to cache
optimize' _ (Free (WritePtr a k)) = optimize' (Just a) k

-- Add to cache
optimize' (Just c) (Free (AddPtr a k)) = optimize' (Just (c+a)) k

-- Combine adds (no cache)
optimize' Nothing (Free (AddPtr a (Free (AddPtr b k)))) = optimize' Nothing (Free (AddPtr (a+b) k))

-- Add 0 (no cache) is a nop.
optimize' Nothing (Free (AddPtr 0 k)) = optimize' Nothing k

-- Add with no cache: emit an add command
optimize' Nothing (Free (AddPtr a k)) = Free (AddPtr a (optimize' Nothing k))

-- If cell cache is 0, then we can skip loops.
optimize' cc@(Just 0) (Free (Loop _ k)) = optimize' cc k

-- Handle special loop forms.
optimize' cc (Free (Loop body k)) =
    case optimize' Nothing body of
        -- [-] or [+] is a write 0
        Free (AddPtr (-1) (Pure ())) -> optimize' (Just 0) k
        Free (AddPtr   1  (Pure ())) -> optimize' (Just 0) k
        -- Otherwise, spill cache before loop and continue
        b' -> spillCache cc $ Free (Loop b' (optimize' Nothing k))

-- Handle all other commands
optimize' cc (Free bf) = Free (optimize' cc <$> bf)

-- Write cell cache at end (end of program doesn't matter, but can also be end of loop body.)
optimize' cc p@(Pure _) = spillCache cc p

-- If there is a value in the cell cache, write it out before continuing.
spillCache :: Maybe Cell -> BF a -> BF a
spillCache (Just c) k = Free (WritePtr c k)
spillCache Nothing  k = k

-----

-- | Produce a string representing the given program.
dumpBF :: BF () -> String
dumpBF = runBF step . (>> return "")
    where
        step (MovePtr n k)  = "MovePtr " ++ show n ++ "; " ++ k
        step (ReadPtr k)    = "ReadPtr; " ++ k 32
        step (WritePtr n k) = "WritePtr " ++ show n ++ "; " ++ k
        step (AddPtr n k)   = "AddPtr +" ++ show n ++ "; " ++ k
        step (GetChar k)    = "GetChar; " ++ k (Just 'A')
        step (PutChar c k)  = "PutChar " ++ show c ++ "; " ++ k
        step (Loop body k)  = "Loop [" ++ dumpBF body ++ "]; " ++ k
