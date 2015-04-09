module Main where

import Control.Monad.State.Strict
import System.Environment
import System.IO

import BrainFree
import Instructions
import Runtime

defaultMemSize :: Int
defaultMemSize = 30000

-- | Evaluate a brainfuck program in the BF monad.
eval :: [Instr] -> BF ()
eval = mapM_ eval1
    where
        eval1 (IMovePtr n)      = movePtr n
        eval1 (IAddPtr off c)   = modPtr off (+ c)
        eval1 (IInput off)      = bfInputM getChr (writePtr off)
        eval1 (IOutput off)     = bfOutputM (readPtr off) putChr
        eval1 (ILoop off body)  = loop off (eval body)
        eval1 (IWritePtr off c) = writePtr off c
        eval1 (IMultPtr dstOff srcOff c) = readPtr srcOff >>= \y -> modPtr dstOff (+ y*c)

-- | Run a bf program in 'IO' using a 'VectorMem' data store.
runVecMem :: [Instr] -> IO ()
runVecMem = withVectorMem defaultMemSize . evalStateT . runBFM step . eval
    where
        step (MovePtr n k)      = modify (vecMove n) >> k
        step (ReadPtr off k)    = get >>= vecRead off >>= k
        step (WritePtr off c k) = get >>= vecWrite off c >> k
        step (GetChar k)        = getc >>= k
        step (PutChar c k)      = putc c >> k
        step (Loop off body k)  = bfLoopM (get >>= vecRead off) (runBFM step body) >> k

-- | Run a bf program in 'IO' using a 'FPtrMem' data store.
runFPtrMem :: [Instr] -> IO ()
runFPtrMem = withFPtrMem defaultMemSize . evalStateT . runBFM step . eval
    where
        step (MovePtr n k)      = modify (fptrMove n) >> k
        step (ReadPtr off k)    = get >>= fptrRead off >>= k
        step (WritePtr off c k) = get >>= fptrWrite off c >> k
        step (GetChar k)        = getc >>= k
        step (PutChar c k)      = putc c >> k
        step (Loop off body k)  = bfLoopM (get >>= fptrRead off) (runBFM step body) >> k

-- | Run a bf program using an infinite 'Tape'.
--
-- To make it interesting, does not use auxiliary monads, but instead
-- pure functions of @'Tape' -> 'String' -> 'String'@.
-- The result is an 'interact'-style @'String' -> 'String'@ function
-- that takes a 'String' for input and produces a 'String' as output.
runTape :: [Instr] -> String -> String
runTape prog = runBF step (eval prog >> return finish) blankTape
    where
        step :: BFF (Tape -> String -> String) -> Tape -> String -> String
        step (MovePtr n k)      t i      = k (tapeMove n t) i
        step (ReadPtr off k)    t i      = k (tapeRead off t) t i
        step (WritePtr off c k) t i      = k (tapeWrite off c t) i
        step (GetChar k)        t (c:cs) = k (Just c) t cs
        step (GetChar k)        t []     = k Nothing  t []
        step (PutChar c k)      t i      = c : k t i
        step (Loop off body k)  t i      = loop' t i
            where
                loop' t' i' = if tapeRead off t' /= 0 then body' t' i' else k t' i'
                body' = runBF step (body >> return loop')

        finish :: Tape -> String -> String
        finish _ _ = ""

-- | Use 'interact' to actually run the result of 'runTape' in 'IO'.
--
-- Note that 'interact' closes the stdin handle at the end, so this
-- can't be used to run multiple programs in the same session.
runTapeIO :: [Instr] -> IO ()
runTapeIO = interact . runTape

-- | Generate C code for a bf program.
generateC :: [Instr] -> IO ()
generateC bf = putStr . unlines $
    [ "/* generated by brainfree */"
    , "unsigned char m[30000], *p = m;"
    , "int main(void) {" ]
    ++ genBlock bf
    ++ [indent "return 0;", "}"]
    where
        indent = ("  " ++)
        genBlock = map indent . concatMap step
        step (IMovePtr n)       = ["p += " ++ show n ++ ";"]
        step (IAddPtr off c)    = ["p[" ++ show off ++ "] += " ++ show c ++ ";"]
        step (IInput off)       = ["p[" ++ show off ++ "] = getchar();"]
        step (IOutput off)      = ["putchar(p[" ++ show off ++ "]);"]
        step (ILoop off body)   = ["while (p[" ++ show off ++ "]) {"]
                                    ++ genBlock body
                                    ++ ["}"]
        step (IWritePtr off c)  = ["p[" ++ show off ++ "] = " ++ show c ++ ";"]
        step (IMultPtr o1 o2 c) = ["p[" ++ show o1 ++ "] += p[" ++ show o2 ++ "] * " ++ show c ++ ";"]

-- | Generate Haskell code for a bf program.
generateHS :: [Instr] -> IO ()
generateHS bf = putStr . unlines $
    [ " -- generated by brainfree"
    , "import Runtime"
    , "import Control.Monad.State.Strict"
    , "import System.IO"
    , "main = hSetBuffering stdout NoBuffering >> run"
    , "mv n = modify $ fptrMove n"
    , "rd off = get >>= fptrRead off"
    , "wr off c = get >>= fptrWrite off c"
    , "add off c = get >>= \\p -> fptrRead off p >>= \\x -> fptrWrite off (c + x) p"
    , "mult o1 o2 c = rd o2 >>= add o1 . (* c)"
    , "run = withFPtrMem " ++ show defaultMemSize ++ ". evalStateT $ do"
    ] ++ genBlock bf
    where
        indent = ("  " ++)
        genBlock = map indent . concatMap step
        step (IMovePtr n)       = ["mv (" ++ show n ++ ")"]
        step (IAddPtr off c)    = ["add (" ++ show off ++ ") " ++ show c]
        step (IInput off)       = ["bfInputM getc (wr (" ++ show off ++ "))"]
        step (IOutput off)      = ["bfOutputM (rd (" ++ show off ++ ")) putc"]
        step (ILoop off body)   = ("bfLoopM (rd (" ++ show off ++ ")) $ do") : genBlock body
        step (IWritePtr off c)  = ["wr (" ++ show off ++ ") " ++ show c]
        step (IMultPtr o1 o2 c) = ["mult (" ++ show o1 ++ ") (" ++ show o2 ++ ") " ++ show c ++ ""]

-- | Handle command line arguments.
processArgs :: ([Instr] -> IO ()) -> Bool -> [String] -> IO ()
processArgs _ o ("-v":args) = processArgs runVecMem o args
processArgs _ o ("-f":args) = processArgs runFPtrMem o args
processArgs _ o ("-t":args) = processArgs runTapeIO o args
processArgs _ o ("-c":args) = processArgs generateC o args
processArgs _ o ("-h":args) = processArgs generateHS o args
processArgs r _ ("-o":args) = processArgs r True args
processArgs _ _ (('-':_):_) = usage
processArgs runner o (filename:_) =
    parseFile filename >>= either print (runner . if o then optimize else id)
processArgs _ _ _ = usage

usage :: IO ()
usage = putStrLn "usage: brainfree [-v|-f|-t] [-o] FILENAME"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    getArgs >>= processArgs runVecMem False
