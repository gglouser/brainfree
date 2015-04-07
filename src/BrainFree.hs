{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}
{-|
A free monad representing an abstract brainfuck machine.
-}
module BrainFree (
  -- * BF monad
  BF, BFF(..)
  , movePtr, addPtr, input, output, loop, writePtr, multPtr
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
import qualified Data.IntMap as Map
import qualified Text.Parsec as P
import Runtime (Cell, Offset)

-- | Alias for the free monad on 'BFF' actions.
type BF = F BFF

-- | A functor representing abstract brainfuck machine actions.
data BFF k
    -- Basic bf commands
    = MovePtr  !Int          k          -- ^ Move the data pointer the specified amount.
    | AddPtr   !Offset !Cell k          -- ^ Add to the cell at offset from data pointer.
    | Input    !Offset       k          -- ^ Accept an input character to cell at offset.
    | Output   !Offset       k          -- ^ Send output character from cell at offset.
    | Loop     (BF ())       k          -- ^ Execute a loop.
    -- Extended bf commands
    | WritePtr !Offset !Cell k          -- ^ Write to the cell at offset from data pointer.
    | MultPtr  !Offset !Offset !Cell k  -- ^ @offset1 += @offset2 * c
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

multPtr :: MonadFree BFF m => Offset -> Offset -> Cell -> m ()
multPtr dstOff srcOff n = liftF $ MultPtr dstOff srcOff n ()

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
optimize = toF . optDeferMoves 0 . optSpecLoops . fromF

-- Check for special loop forms.
optSpecLoops :: Free BFF a -> Free BFF a
optSpecLoops (Free (Loop body k)) =
    let body' = fromF body
        k' = optSpecLoops k
    in case checkMultLoop 0 Map.empty body' of
        Just ms -> genMults ms k'
        Nothing -> Free (Loop (toF $ optSpecLoops body') k')
optSpecLoops (Free bf) = Free (optSpecLoops <$> bf)
optSpecLoops p = p

checkMultLoop :: Int -> Map.IntMap Cell -> Free BFF a -> Maybe (Map.IntMap Cell)
checkMultLoop p xs (Free (MovePtr n k)) = checkMultLoop (p+n) xs k
checkMultLoop p xs (Free (AddPtr o c k)) = checkMultLoop p (Map.insertWith (+) (p+o) c xs) k
checkMultLoop 0 xs (Pure _) =
    case Map.lookup 0 xs of
        Just 1 -> Just xs
        Just (-1) -> Just xs
        _ -> Nothing
checkMultLoop _ _ _ = Nothing

genMults :: Map.IntMap Cell -> Free BFF a -> Free BFF a
genMults ms cont = Map.foldrWithKey mult (Free (WritePtr 0 0 cont)) ms
    where
        mult 0 _ k = k
        mult p v k = Free (MultPtr p 0 v k)

-- Defer moves
-- This also happens during parse but more can be done
-- after transforming special loops.
optDeferMoves :: Int -> Free BFF a -> Free BFF a
optDeferMoves p (Free (MovePtr n k))  = optDeferMoves (p+n) k
optDeferMoves p (Free (AddPtr o c k)) = Free (AddPtr (p+o) c (optDeferMoves p k))
optDeferMoves p (Free (Input o k))    = Free (Input  (p+o)   (optDeferMoves p k))
optDeferMoves p (Free (Output o k))   = Free (Output (p+o)   (optDeferMoves p k))
optDeferMoves p (Free (Loop body k))  = emitMove p $
    Free (Loop (toF . optDeferMoves 0 $ fromF body) (optDeferMoves 0 k))
optDeferMoves p (Free (WritePtr o c k)) = Free (WritePtr (p+o) c (optDeferMoves p k))
optDeferMoves p (Free (MultPtr o1 o2 c k)) = Free (MultPtr (p+o1) (p+o2) c (optDeferMoves p k))
optDeferMoves p end@(Pure _) = emitMove p end

emitMove :: Int -> Free BFF a -> Free BFF a
emitMove 0 k = k
emitMove p k = Free (MovePtr p k)

-----

-- | Produce a string representing the given program.
dumpBF :: BF () -> String
dumpBF = runBF step . (>> return "")
    where
        step (MovePtr n k)      = "MovePtr " ++ show n ++ ";\n" ++ k
        step (AddPtr off n k)   = "AddPtr @" ++ show off ++ " += " ++ show n ++ ";\n" ++ k
        step (Input off k)      = "Input @" ++ show off ++ ";\n" ++ k
        step (Output off k)     = "Output @" ++ show off ++ ";\n" ++ k
        step (Loop body k)      = "Loop {\n" ++ dumpBF body ++ "}\n" ++ k
        step (WritePtr off n k) = "WritePtr @" ++ show off ++ " = " ++ show n ++ ";\n" ++ k
        step (MultPtr o1 o2 n k) = "MultPtr @" ++ show o1 ++ " += @" ++ show o2 ++ " * " ++ show n ++ ";\n" ++ k
