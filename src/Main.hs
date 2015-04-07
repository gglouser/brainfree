module Main where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Monoid
import System.Environment
import System.IO

import BrainFree
import Runtime

defaultMemSize :: Int
defaultMemSize = 30000

-- | Run a 'BF' in 'IO' using a 'VectorMem' data store.
runVecMem :: BF () -> IO ()
runVecMem = withVectorMem defaultMemSize . evalStateT . runBFM step
    where
        rd off   = get >>= vecRead off
        wr off n = get >>= vecWrite off n

        step (MovePtr n k)      = modify (vecMove n) >> k
        step (AddPtr off c k)   = rd off >>= wr off . (+ c) >> k
        step (Input off k)      = bfInputM getc (wr off) >> k
        step (Output off k)     = bfOutputM (rd off) putc >> k
        step (Loop body k)      = bfLoopM (rd 0) (runBFM step body) >> k
        step (WritePtr off c k) = wr off c >> k
        step (MultPtr o1 o2 c k) = ((+) <$> rd o1 <*> ((* c) <$> rd o2)) >>= wr o1 >> k

-- | Run a 'BF' in 'IO' using a 'FPtrMem' data store.
runFPtrMem :: BF () -> IO ()
runFPtrMem = withFPtrMem defaultMemSize . evalStateT . runBFM step
    where
        rd off   = get >>= fptrRead off
        wr off n = get >>= fptrWrite off n

        step (MovePtr n k)      = modify (fptrMove n) >> k
        step (AddPtr off c k)   = rd off >>= wr off . (+ c) >> k
        step (Input off k)      = bfInputM getc (wr off) >> k
        step (Output off k)     = bfOutputM (rd off) putc >> k
        step (Loop body k)      = bfLoopM (rd 0) (runBFM step body) >> k
        step (WritePtr off c k) = wr off c >> k
        step (MultPtr o1 o2 c k) = ((+) <$> rd o1 <*> ((* c) <$> rd o2)) >>= wr o1 >> k


-- | Run a 'BF' using an infinite 'Tape'.
--
-- To make it interesting, does not use auxiliary monads, but instead
-- endomorphisms of @'Tape' -> 'Offset' -> 'String' -> 'String'@.
-- The result is an 'interact'-style @'String' -> 'String'@ function
-- that takes a 'String' for input and produces a 'String' as output.
runTape :: BF () -> String -> String
runTape prog = appEndo (toTapeAction prog) finish blankTape 0
    where
        toTapeAction :: BF () -> TapeAction
        toTapeAction bf = runBF step (bf >> return mempty)

        step :: BFF (TapeAction) -> TapeAction
        step (MovePtr n k)    = changeRefPos n <> k
        step (AddPtr off c k) = moveToOffset off <> modHead (+ c) <> k
        step (Input off k)    = moveToOffset off <> tapeInput <> k
        step (Output off k)   = moveToOffset off <> tapeOutput <> k
        step (Loop body k)    = loop' <> k
            where loop' = moveToOffset 0 <> whenNZ (toTapeAction body <> loop')
        step (WritePtr off c k) = moveToOffset off <> modHead (const c) <> k
        step (MultPtr o1 o2 c k) = moveToOffset o1 <> tapeMult (o2-o1) c <> k

        finish :: Tape -> Offset -> String -> String
        finish _ _ _ = ""

type TapeAction = Endo (Tape -> Offset -> String -> String)

-- | Change the tape reference position (no need to move the tape itself).
changeRefPos :: Int -> TapeAction
changeRefPos n = Endo $ \k t p i -> k t (p - n) i

-- | Move the tape to an offset relative to the current reference position.
moveToOffset :: Offset -> TapeAction
moveToOffset off = Endo $ \k t p i -> k (tapeMove (off - p) t) off i

-- | Apply the function to the cell at the tape head.
modHead :: (Cell -> Cell) -> TapeAction
modHead f = Endo $ \k t p i -> k (tapeWrite (f (tapeRead t)) t) p i

-- | Take input. Does not change tape if input is empty.
tapeInput :: TapeAction
tapeInput = Endo $ \k t p i ->
    case i of
        c : cs -> appEndo (modHead . const $ coerce c) k t p cs
        []     -> k t p []

-- | Output the cell at the tape head.
tapeOutput :: TapeAction
tapeOutput = Endo $ \k t p i -> coerce (tapeRead t) : k t p i

-- | Execute the given tape action when the cell at tape head is non-zero.
whenNZ :: TapeAction -> TapeAction
whenNZ ta = Endo $ \k t p i ->
    if tapeRead t /= 0
        then appEndo ta k t p i
        else k t p i

-- | Add n * value at offset to head.
tapeMult :: Int -> Cell -> TapeAction
tapeMult off n = Endo $ \k t p i ->
    let x = tapeRead (tapeMove off t)
    in appEndo (modHead (+ x*n)) k t p i

-- | Use 'interact' to actually run the result of 'runTape' in 'IO'.
--
-- Note that 'interact' closes the stdin handle at the end, so this
-- can't be used to run multiple programs in the same session.
runTapeIO :: BF () -> IO ()
runTapeIO = interact . runTape


generateC :: BF () -> IO ()
generateC bf = do
    putStrLn "/* generated by brainfree */"
    putStrLn "unsigned char m[30000], *p = m;"
    putStrLn "int main(void) {"
    putStr $ runBF (step "  ") (bf >> return "")
    putStrLn "  return 0;\n}\n"
    where
        step ind (MovePtr n k)      = ind ++ "p += " ++ show n ++ ";\n" ++ k
        step ind (AddPtr off c k)   = ind ++ "p[" ++ show off ++ "] += " ++ show c ++ ";\n" ++ k
        step ind (Input off k)      = ind ++ "p[" ++ show off ++ "] = getchar();\n" ++ k
        step ind (Output off k)     = ind ++ "putchar(p[" ++ show off ++ "]);\n" ++ k
        step ind (Loop body k)      = ind ++ "while (p[0]) {\n" ++ runBF (step (ind ++ "  ")) (body >> return "") ++ ind ++ "}\n" ++ k
        step ind (WritePtr off c k) = ind ++ "p[" ++ show off ++ "] = " ++ show c ++ ";\n" ++ k
        step ind (MultPtr o1 o2 c k) = ind ++ "p[" ++ show o1 ++ "] += p[" ++ show o2 ++ "] * " ++ show c ++ ";\n" ++ k

generateHS :: BF () -> IO ()
generateHS bf = do
    putStrLn "import Control.Monad.State.Strict"
    putStrLn "import System.IO"
    putStrLn "import Runtime"
    putStrLn "rd off = get >>= fptrRead off"
    putStrLn "wr off n = get >>= fptrWrite off n"
    putStrLn "main = hSetBuffering stdout NoBuffering >> run"
    putStrLn "run = withFPtrMem 30000 . evalStateT $ do {"
    putStr $ runBF step (bf >> return "")
    putStrLn "}"
    where
        step (MovePtr n k)      = "modify (fptrMove (" ++ show n ++ "));\n" ++ k
        step (AddPtr off c k)   = "rd (" ++ show off ++ ") >>= wr (" ++ show off ++ ") . (+ (" ++ show c ++ "));\n" ++ k
        step (Input off k)      = "bfInputM getc (wr (" ++ show off ++ "));\n" ++ k
        step (Output off k)     = "bfOutputM (rd (" ++ show off ++ ")) putc;\n" ++ k
        step (Loop body k)      = "bfLoopM (rd 0) (do {\n" ++ runBF step (body >> return "") ++ "});\n" ++ k
        step (WritePtr off c k) = "wr (" ++ show off ++ ") (" ++ show c ++ ");\n" ++ k
        step (MultPtr o1 o2 c k) = "((+) <$> rd " ++ show o1 ++ " <*> ((* " ++ show c ++ ") <$> rd " ++ show o2 ++ ")) >>= wr " ++ show o1 ++ ";\n" ++ k


-- | Handle command line arguments.
processArgs :: (BF () -> IO ()) -> Bool -> [String] -> IO ()
processArgs _ o ("-v":args) = processArgs runVecMem o args
processArgs _ o ("-f":args) = processArgs runFPtrMem o args
processArgs _ o ("-t":args) = processArgs runTapeIO o args
processArgs _ o ("-c":args) = processArgs generateC o args
processArgs _ o ("-h":args) = processArgs generateHS o args
processArgs r _ ("-o":args) = processArgs r True args
processArgs _ _ (('-':_):_) = usage
processArgs runner o (filename:_) =
    parseFile filename >>= either print (if o then runner . optimize else runner)
processArgs _ _ _ = usage

usage :: IO ()
usage = putStrLn "usage: brainfree [-v|-f|-t] [-o] FILENAME"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    getArgs >>= processArgs runVecMem False
