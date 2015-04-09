{-|
Extended brainfuck instruction representation.
-}
module Instructions (
  -- * Instruction type
  Instr(..)
  -- * Parsing
  , parseString, parseFile
  -- * Optimizer
  , optimize
  ) where

import Control.Applicative
import Control.Monad (guard, mzero)
import Data.Bifunctor (first)
import Data.Foldable (foldlM)
import qualified Data.IntMap as Map
import qualified Text.Parsec as P
import Runtime (Cell, Offset)

-- | Extended brainfuck instruction set.
data Instr
    -- Basic bf commands
    = IMovePtr  !Offset                 -- ^ Move the data pointer.
    | IAddPtr   !Offset !Cell           -- ^ Add to cell at offset from data pointer.
    | IInput    !Offset                 -- ^ Accept an input character to cell at offset.
    | IOutput   !Offset                 -- ^ Send output character from cell at offset.
    | ILoop     !Offset [Instr]         -- ^ Execute a loop, using cell at offset for condition.
    -- Extended bf commands
    | IWritePtr !Offset !Cell           -- ^ Write to cell at offset from data pointer.
    | IMultPtr  !Offset !Offset !Cell   -- ^ @offset1 += @offset2 * c
    deriving Show


-- | Parse a string containing a brainfuck program and return either
-- an error message or the program.
parseString :: String -> Either String [Instr]
parseString = parseBF ""

-- | Parse the contents of the named file as a brainfuck program and
-- return either an error message or the program.
parseFile :: FilePath -> IO (Either String [Instr])
parseFile filename = parseBF filename <$> readFile filename

parseBF :: String -> String -> Either String [Instr]
parseBF filename = first show . P.parse (skipChars *> parse' <* P.eof) filename

parse' :: P.Parsec String () [Instr]
parse' = many parse1

parse1 :: P.Parsec String () Instr
parse1 = IMovePtr . sum <$> some (1 <$ tok '>' <|> (-1) <$ tok '<')
     <|> IAddPtr 0 . sum <$> some (1 <$ tok '+' <|> (-1) <$ tok '-')
     <|> IInput 0 <$ tok ','
     <|> IOutput 0 <$ tok '.'
     <|> ILoop 0 <$> P.between (tok '[') (tok ']') parse'
    where
        tok c = P.char c <* skipChars

skipChars :: P.Parsec String u ()
skipChars = P.skipMany $ P.noneOf "<>-+,.[]"


-- | Optimize a sequence of instructions.
optimize :: [Instr] -> [Instr]
optimize = optDeferMoves . optSpecLoops

-- Check for special loop forms.
-- Currently limited to multiplication loops (includes clear and copy loops).
optSpecLoops :: [Instr] -> [Instr]
optSpecLoops = concatMap optSpecLoop'

optSpecLoop' :: Instr -> [Instr]
optSpecLoop' (ILoop off body) =
    case checkConstLoop body of
        Just ms -> genMults off ms
        Nothing -> [ILoop off (optSpecLoops body)]
optSpecLoop' i = [i]

-- A /const loop/ is a loop that modifies a set of cells by a constant
-- amount and has a net pointer movement of zero. Also, the cell at offset 0 must
-- be modified by exactly 1 or -1.
checkConstLoop :: [Instr] -> Maybe (Map.IntMap Cell)
checkConstLoop prog = do
    (p, xs) <- foldlM ccl (0, Map.empty) prog
    guard $ p == 0
    z <- Map.lookup 0 xs
    case z of
        -1 -> return xs
        1  -> return $ Map.map negate xs
        _  -> mzero
    where
        ccl (p, xs) (IMovePtr n)  = return (p + n, xs)
        ccl (p, xs) (IAddPtr o c) = return (p, Map.insertWith (+) (p + o) c xs)
        ccl _       _             = mzero

genMults :: Offset -> Map.IntMap Cell -> [Instr]
genMults off = Map.foldrWithKey mult [IWritePtr off 0]
    where
        mult 0 _ k = k
        mult p v k = IMultPtr (p+off) off v : k

-- Defer moves
optDeferMoves :: [Instr] -> [Instr]
optDeferMoves = defer False 0
    where
        defer inLoop p prog =
            let (q, chunk) = foldl dfr (p, id) prog
                end = if inLoop && p /= q
                        then [IMovePtr (q - p)]
                        else []
            in chunk end

        dfr (p, pre) (IMovePtr n)       = (p + n, pre)
        dfr (p, pre) i                  = (p, pre . (addOffset p i :))
        
        addOffset p (IAddPtr o c)      = IAddPtr (p + o) c
        addOffset p (IInput o)         = IInput (p + o)
        addOffset p (IOutput o)        = IOutput (p + o)
        addOffset p (ILoop o body)     = ILoop (p + o) (defer True p body)
        addOffset p (IWritePtr o c)    = IWritePtr (p + o) c
        addOffset p (IMultPtr o1 o2 c) = IMultPtr (p + o1) (p + o2) c
        addOffset _ i_no_offset        = i_no_offset
