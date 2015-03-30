{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}
module BrainFree where

import Control.Applicative
import Control.Monad.Free
import Control.Monad.Free.Church (fromF)
import Data.Bifunctor
import qualified Text.Parsec as P
import Runtime (Cell, coerce)

type BF = Free BFF

data BFF k = MovePtr !Int k
           | ReadPtr (Cell -> k)
           | WritePtr !Cell k
           | AddPtr !Cell k                 -- allows more optimizations
           | GetChar (Maybe Char -> k)      -- Nothing indicates end-of-file
           | PutChar !Char k
           | Loop (BF ()) k
    deriving Functor

movePtr :: MonadFree BFF m => Int -> m ()
movePtr n = liftF (MovePtr n ())

readPtr :: MonadFree BFF m => m Cell
readPtr = liftF (ReadPtr id)

writePtr :: MonadFree BFF m => Cell -> m ()
writePtr c = liftF (WritePtr c ())

addPtr :: MonadFree BFF m => Cell -> m ()
addPtr n = liftF (AddPtr n ())
-- addPtr n = readPtr >>= writePtr . (+ n)

getChr :: MonadFree BFF m => m (Maybe Char)
getChr = liftF (GetChar id)

putChr :: MonadFree BFF m => Char -> m ()
putChr c = liftF (PutChar c ())

loop :: MonadFree BFF m => BF () -> m ()
loop body = liftF (Loop body ())

-- do nothing on EOF
getCell :: MonadFree BFF m => m ()
getCell = getChr >>= maybe (return ()) (writePtr . coerce)

putCell :: MonadFree BFF m => m ()
putCell = readPtr >>= putChr . coerce

runBF :: (BFF a -> a) -> BF a -> a
runBF = iter

runBFM :: Monad m => (BFF (m a) -> m a) -> BF a -> m a
runBFM = iterM

-----

parseBF :: P.Parsec String () (BF ())
parseBF = parse' <* P.eof

parse' :: P.Parsec String () (BF ())
-- parse' = sequence_ <$> many parse1
-- parse' = improve . sequence_ <$> many parse1
parse' = fromF . sequence_ <$> many parse1

parse1 :: MonadFree BFF m => P.Parsec String () (m ())
parse1 = '<' ~> movePtr (-1)
     <|> '>' ~> movePtr 1
     <|> '-' ~> addPtr (-1)
     <|> '+' ~> addPtr 1
     <|> ',' ~> getCell
     <|> '.' ~> putCell
     <|> loop <$> P.between (P.char '[') (P.char ']') parse'
     <|> return () <$ P.noneOf "]"
    where c ~> i = i <$ P.char c

parseString :: String -> Either String (BF ())
parseString = first show . P.parse parseBF ""

parseFile :: FilePath -> IO (Either String (BF ()))
parseFile filename = do
    src <- readFile filename
    return . first show $ P.parse parseBF filename src

-----

optimize :: BF a -> BF a
optimize = optimize' (Just 0)

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

spillCache :: Maybe Cell -> Free BFF a -> Free BFF a
spillCache (Just c) k = Free (WritePtr c k)
spillCache Nothing  k = k

-----

dumpBF :: BF () -> String
dumpBF = runBF step . (>> return "")
    where
        step (MovePtr n k)  = "MovePtr " ++ show n ++ "; " ++ k
        step (ReadPtr k)    = "ReadPtr; " ++ k 0
        step (WritePtr n k) = "WritePtr " ++ show n ++ "; " ++ k
        step (AddPtr n k)   = "AddPtr +" ++ show n ++ "; " ++ k
        step (GetChar k)    = "GetChar; " ++ k (Just 'A')
        step (PutChar c k)  = "PutChar " ++ show c ++ "; " ++ k
        step (Loop body k)  = "Loop [" ++ dumpBF body ++ "]; " ++ k
