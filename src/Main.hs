module Main where

import Control.Monad.State.Strict
import System.Environment
import System.IO

import BrainFree
import Runtime

defaultMemSize :: Int
defaultMemSize = 30000

runVecMem :: BF () -> IO ()
runVecMem = withVectorMem defaultMemSize . evalStateT . runBFM step
    where
        step (MovePtr n k)  = modify (vecMove n) >> k
        step (ReadPtr k)    = get >>= vecRead >>= k
        step (WritePtr c k) = get >>= vecWrite c >> k
        step (AddPtr c k)   = get >>= \vm -> vecRead vm >>= flip vecWrite vm . (+ c) >> k
        step (GetChar k)    = getc >>= k
        step (PutChar c k)  = putc c >> k
        step (Loop body k)  = bfLoopM (get >>= vecRead) (runBFM step body) >> k

runFPtrMem :: BF () -> IO ()
runFPtrMem = withFPtrMem defaultMemSize . evalStateT . runBFM step
    where
        step (MovePtr n k)  = modify (fptrMove n) >> k
        step (ReadPtr k)    = get >>= fptrRead >>= k
        step (WritePtr c k) = get >>= fptrWrite c >> k
        step (AddPtr c k)   = get >>= \p -> fptrRead p >>= flip fptrWrite p . (+ c) >> k
        step (GetChar k)    = getc >>= k
        step (PutChar c k)  = putc c >> k
        step (Loop body k)  = bfLoopM (get >>= fptrRead) (runBFM step body) >> k

runTape :: BF () -> String -> String
runTape prog = runBF step (prog >> return finish) blankTape
    where
        step :: BFF (Tape -> String -> String) -> Tape -> String -> String
        step (MovePtr n k)  t i      = k (tapeMove n t) i
        step (ReadPtr k)    t i      = k (tapeRead t) t i
        step (WritePtr c k) t i      = k (tapeWrite c t) i
        step (AddPtr c k)   t i      = k (tapeWrite (tapeRead t + c) t) i
        step (GetChar k)    t (c:cs) = k (Just c) t cs
        step (GetChar k)    t []     = k Nothing t []
        step (PutChar c k)  t i      = c : k t i
        step (Loop body k)  t i      = loop' t i
            where
                loop'       t' i'    = if tapeRead t' == 0 then k t' i' else body' t' i'
                body' = runBF step (body >> return loop')

        finish :: Tape -> String -> String
        finish _ _ = ""

runTapeIO :: BF () -> IO ()
runTapeIO = interact . runTape

processArgs :: (BF () -> IO ()) -> Bool -> [String] -> IO ()
processArgs _ _ [] = return ()
processArgs _ o ("-v":args) = processArgs runVecMem o args
processArgs _ o ("-f":args) = processArgs runFPtrMem o args
processArgs _ o ("-t":args) = processArgs runTapeIO o args
processArgs r _ ("-o":args) = processArgs r True args
processArgs runner o (filename:_) =
    parseFile filename >>= either print (if o then runner . optimize else runner)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    getArgs >>= processArgs runVecMem False
