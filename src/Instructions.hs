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
import Data.Bifunctor
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
optimize = optDeferMoves 0 0 . optSpecLoops

-- Check for special loop forms.
-- Currently limited to multiplication loops (includes clear and copy loops).
optSpecLoops :: [Instr] -> [Instr]
optSpecLoops = concatMap optSpecLoop'

optSpecLoop' :: Instr -> [Instr]
optSpecLoop' (ILoop off body) =
    case checkConstLoop 0 Map.empty body of
        Just ms -> genMults off ms
        Nothing -> [ILoop off (optSpecLoops body)]
optSpecLoop' i = [i]

-- A /const loop/ is a loop that modifies a set of cells by a constant
-- amount and has a net pointer movement of zero. Also, the cell at offset 0 must
-- be modified by exactly 1 or -1.
checkConstLoop :: Int -> Map.IntMap Cell -> [Instr] -> Maybe (Map.IntMap Cell)
checkConstLoop p xs (IMovePtr n : k) = checkConstLoop (p+n) xs k
checkConstLoop p xs (IAddPtr o c : k) = checkConstLoop p (Map.insertWith (+) (p+o) c xs) k
checkConstLoop 0 xs [] =
    case Map.lookup 0 xs of
        Just 1 -> Just xs
        Just (-1) -> Just xs
        _ -> Nothing
checkConstLoop _ _ _ = Nothing

genMults :: Offset -> Map.IntMap Cell -> [Instr]
genMults off = Map.foldrWithKey mult [IWritePtr off 0]
    where
        mult 0 _ k = k
        mult p v k = IMultPtr (p+off) 0 v : k

-- Defer moves
optDeferMoves :: Int -> Int -> [Instr] -> [Instr]
optDeferMoves p b (IMovePtr n : k)  = optDeferMoves (p+n) b k
optDeferMoves p b (IAddPtr o c : k) = IAddPtr (p+o) c : optDeferMoves p b k
optDeferMoves p b (IInput o : k)    = IInput  (p+o)   : optDeferMoves p b k
optDeferMoves p b (IOutput o : k)   = IOutput (p+o)   : optDeferMoves p b k
optDeferMoves p b (ILoop o body : k) =
    ILoop (p+o) (optDeferMoves p p body) : optDeferMoves p b k
optDeferMoves p b (IWritePtr o c : k) = IWritePtr (p+o) c : optDeferMoves p b k
optDeferMoves p b (IMultPtr o1 o2 c : k) = IMultPtr (p+o1) (p+o2) c : optDeferMoves p b k
optDeferMoves p b [] = case p - b of
                        0 -> []
                        n -> [IMovePtr n]
