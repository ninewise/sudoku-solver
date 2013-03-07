--------------------------------------------------------------------------------
import           Control.Monad (replicateM, msum, replicateM_)
import           Data.Char     (isNumber, digitToInt)
import           Data.Maybe    (fromJust)
import qualified Data.Map      as M
import qualified Data.Foldable as F

--------------------------------------------------------------------------------
-- Cell represents a cell in the sudoku grid. Contains a possibility of digits,
-- a certain digit or is invalid.
data Cell = E [Int] | D Int | X
    deriving (Eq)

instance Show Cell where
    show (E l)  = "E " ++ concatMap show l
    show (D n)  = show n

--------------------------------------------------------------------------------
-- Tile is the location of a cell in the sudoku grid.
data Tile = Tile Int Int
    deriving (Eq, Show, Ord)

--------------------------------------------------------------------------------
-- A sudoku maps the 81 tiles on their cells.
type Sudoku = M.Map Tile Cell

--------------------------------------------------------------------------------
-- Read a sudoku from stdin. Format: 9 lines of 9 characters each, where a digit
-- is parsed as that digit, and any other character as an empty cell.
parseSudoku :: IO Sudoku
parseSudoku = do
    lines <- replicateM 9 getLine
    return $ M.fromList $
        [ (Tile r c, parseCell char)
        | (r, line) <- zip [0..] lines
        , (c, char) <- zip [0..] line ]
  where
    parseCell char | isNumber char  = D $ digitToInt char
                   | otherwise      = E [1..9]

--------------------------------------------------------------------------------
-- fixTile assumes the given tile is correct, and removes all clashing
-- possibilities. If there's a clash with a certain digit, Nothing is returned.
fixTile :: Tile -> Sudoku -> Maybe Sudoku
fixTile (Tile r c) s = validate $ M.mapWithKey (filterCell $ s M.! Tile r c) s
  where
    dangerTile :: Tile -> Bool
    dangerTile (Tile y x) 
      | c == x && r == y    = False
      | c == x || r == y    = True
      | div r 3 == div y 3 && div c 3 == div x 3
                            = True
      | otherwise           = False

    filterCell :: Cell -> Tile -> Cell -> Cell
    filterCell (D a) t (D b)
      | a == b && dangerTile t  = X
      | otherwise               = D b
    filterCell (D a) t (E ds)
      | dangerTile t            = E $ filter (/= a) ds
      | otherwise               = E ds
    filterCell (E ds) _ c       = c

    validate :: Sudoku -> Maybe Sudoku
    validate s = if F.any (== X) s then Nothing else Just s

--------------------------------------------------------------------------------
-- Calls fixTile for every tile in the Sudoku. This method leaves only valid
-- possibilities, and returns Nothing if there are clashing values in the
-- Sudoku.
iterateFixes :: Sudoku -> Maybe Sudoku
iterateFixes sudoku = F.foldrM fixTile sudoku (M.keys sudoku)

--------------------------------------------------------------------------------
-- Makes all tiles where there is only one possible digit left fixed.
fixSingles :: Sudoku -> Sudoku
fixSingles = M.map go
  where
    go (E [n]) = D n
    go x       = x

--------------------------------------------------------------------------------
-- Repeates removing possibilities and fixing cells with only one possible digit
-- until no further change is made this way.
fillSudoku :: Sudoku -> Sudoku
fillSudoku sudoku = fst $ until noChange nextIteration (M.empty, sudoku)
  where
    noChange (a, b) = a == b
    nextIteration (_, old) = (old, fixSingles $ fromJust $ iterateFixes old)

--------------------------------------------------------------------------------
-- Solves the sudoku, but returns Nothing if the sudoku is invalid.
solveSudoku :: Sudoku -> Maybe Sudoku
solveSudoku = go (Tile 0 0) . return
  where
    go :: Tile -> Maybe Sudoku -> Maybe Sudoku
    go _ Nothing = Nothing
    go (Tile r c) (Just s)
      | c >= 9      = go (Tile (r + 1) 0) $ Just s
      | r >= 9      = Just s
      | otherwise   = case s M.! (Tile r c) of
            E list -> let
                        tile = Tile r c
                        test n = go (Tile r $ c + 1) $
                                    fixTile tile $ M.insert tile (D n) s
                      in msum $ map test list
            D n -> go (Tile r $ c + 1) $ Just s

--------------------------------------------------------------------------------
-- The main method. Reads a number of sudoku's from stdin and prints their
-- solutions.
main :: IO ()
main = do
    times <- (readLn :: IO Int)
    replicateM_ times $ do 
        sudoku <- parseSudoku
        print $ solveSudoku sudoku
        _ <- getLine
        putStrLn ""
    
