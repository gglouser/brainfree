{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}
{-|
A free monad representing an abstract brainfuck machine.
-}
module BrainFree (
  -- * BF monad
  BF, BFF(..)
  , movePtr, addPtr, input, output, loop, writePtr
  , runBF, runBFM
  -- * Parsing
  , parseString, parseFile
  -- * Optimizer
  , optimize
  -- * Utilities
  , dumpBF
  ) where

import Control.Applicative
import Control.Monad.Free (Free(..))
import Control.Monad.Free.Church
import Data.Bifunctor
import qualified Text.Parsec as P
import Runtime (Cell, Offset)

-- | Alias for the free monad on 'BFF' actions.
type BF = F BFF

-- | A functor representing abstract brainfuck machine actions.
data BFF k
    -- Basic bf commands
    = MovePtr  !Int          k      -- ^ Move the data pointer the specified amount.
    | AddPtr   !Offset !Cell k      -- ^ Add to the cell at offset from data pointer.
    | Input    !Offset       k      -- ^ Accept an input character to cell at offset.
    | Output   !Offset       k      -- ^ Send output character from cell at offset.
    | Loop     (BF ())       k      -- ^ Execute a loop.
    -- Extended bf commands
    | WritePtr !Offset !Cell k      -- ^ Write to the cell at offset from data pointer.
    deriving Functor

movePtr :: MonadFree BFF m => Int -> m ()
movePtr n = liftF $ MovePtr n ()

addPtr :: MonadFree BFF m => Offset -> Cell -> m ()
addPtr off n = liftF $ AddPtr off n ()

input :: MonadFree BFF m => Offset -> m ()
input off = liftF $ Input off ()

output :: MonadFree BFF m => Offset -> m ()
output off = liftF $ Output off ()

loop :: MonadFree BFF m => BF () -> m ()
loop body = liftF $ Loop body ()

writePtr :: MonadFree BFF m => Offset -> Cell -> m ()
writePtr off n = liftF $ WritePtr off n ()

-- | Tear down a 'BF' program using the given function to handle 'BFF' actions
-- ('iter' specialized to 'BFF').
runBF :: (BFF a -> a) -> BF a -> a
runBF f p = runF p id f

-- | Tear down a 'BF' program in a 'Monad' using the given function to handle
-- 'BFF' actions ('iterM' specialized to 'BFF').
runBFM :: Monad m => (BFF (m a) -> m a) -> BF a -> m a
runBFM = iterM

-----

parseBF :: String -> String -> Either String (BF ())
parseBF filename = first show . P.runParser (skipChars *> parse' <* P.eof) 0 filename

parse' :: P.Parsec String Int (BF ())
parse' = do
    chunk <- sequence_ <$> many parse1
    end <- resetOffset
    return $ chunk >> end

parse1 :: MonadFree BFF m => P.Parsec String Int (m ())
parse1 = moves
     <|> adds
     <|> input  <$> getOffset <* tok ','
     <|> output <$> getOffset <* tok '.'
     <|> (>>) <$> resetOffset <*> (loop <$> P.between (tok '[') (tok ']') parse')
    where
        tok c = P.char c <* skipChars
        getOffset = P.getState
        moves = do
            n <- sum <$> some (1 <$ tok '>' <|> (-1) <$ tok '<')
            P.modifyState (+ n)
            return (return ())
        adds = do
            n <- sum <$> some (1 <$ tok '+' <|> (-1) <$ tok '-')
            if n == 0 then return (return ()) else addPtr <$> getOffset <*> pure n

skipChars :: P.Parsec String u ()
skipChars = P.skipMany $ P.noneOf "<>-+,.[]"

resetOffset :: MonadFree BFF m => P.Parsec s Int (m ())
resetOffset = do
    offset <- P.getState
    P.setState 0
    return $ if offset == 0 then return () else movePtr offset

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
optimize :: BF a -> BF a
optimize = toF . optSpecLoops . fromF

-- Check for special loop forms.
optSpecLoops :: Free BFF a -> Free BFF a
optSpecLoops (Free (Loop body k)) =
    case optSpecLoops (fromF body) of
        Free (AddPtr 0 (-1) (Pure ())) -> Free (WritePtr 0 0 (optSpecLoops k))
        Free (AddPtr 0 ( 1) (Pure ())) -> Free (WritePtr 0 0 (optSpecLoops k))
        b' -> Free (Loop (toF b') (optSpecLoops k))
optSpecLoops (Free bf) = Free (optSpecLoops <$> bf)
optSpecLoops p = p

-----

-- | Produce a string representing the given program.
dumpBF :: BF () -> String
dumpBF = runBF step . (>> return "")
    where
        step (MovePtr n k)      = "MovePtr " ++ show n ++ ";\n" ++ k
        step (AddPtr off n k)   = "AddPtr @" ++ show off ++ " +" ++ show n ++ ";\n" ++ k
        step (Input off k)      = "Input @" ++ show off ++ ";\n" ++ k
        step (Output off k)     = "Output @" ++ show off ++ ";\n" ++ k
        step (Loop body k)      = "Loop {\n" ++ dumpBF body ++ "}\n" ++ k
        step (WritePtr off n k) = "WritePtr @" ++ show off ++ " " ++ show n ++ ";\n" ++ k
