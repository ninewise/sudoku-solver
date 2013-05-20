--------------------------------------------------------------------------------
import           Control.Monad (msum, replicateM, replicateM_)
import           Data.Char     (digitToInt, isNumber)
import qualified Data.Foldable as F
import qualified Data.Map      as M
import           Data.Maybe    (fromJust)
import Data.Function (on)

-- ghc --make -auto-all -caf-all -rtsopts -prof -O2 -fforce-recomp \
--  sudoku-solver.hs
-- ./sudoku-solver < sudoku.in +RTS -prof -sstderr

--------------------------------------------------------------------------------
-- Cell represents a cell in the sudoku grid. Contains a possibility of digits,
-- a certain digit or is invalid.
data Cell = E [Int] | D Int | X
    deriving (Eq)

instance Show Cell where
    show (E l)  = "E " ++ concatMap show l
    show (D n)  = show n
    show (X)    = show "X"

--------------------------------------------------------------------------------
-- Tile is the location of a cell in the sudoku grid.
data Tile = Tile Int Int
    deriving (Eq, Show, Ord)

--------------------------------------------------------------------------------
-- A sudoku maps the 81 tiles on their cells.
type Sudoku = M.Map Tile Cell

--------------------------------------------------------------------------------
-- the counter
type Counter = (Int, Int, Int)
incFixTile, incNextIteration, incGo :: Counter -> Counter
incFixTile       (x, y, z) = (x + 1, y, z)
incNextIteration (x, y, z) = (x, y + 1, z)
incGo            (x, y, z) = (x, y, z + 1)

--------------------------------------------------------------------------------
-- Read a sudoku from stdin. Format: 9 lines of 9 characters each, where a digit
-- is parsed as that digit, and any other character as an empty cell.
parseSudoku :: IO Sudoku
parseSudoku = do
    lines <- replicateM 9 getLine
    return $ M.fromList
        [ (Tile r c, parseCell char)
        | (r, line) <- zip [0..] lines
        , (c, char) <- zip [0..] line ]
  where
    parseCell char | isNumber char  = D $ digitToInt char
                   | otherwise      = E [1..9]

--------------------------------------------------------------------------------
-- fixTile assumes the given tile is correct, and removes all clashing
-- possibilities. If there's a clash with a certain digit, Nothing is returned.
fixTile :: Tile -> (Maybe Sudoku, Counter) -> (Maybe Sudoku, Counter)
fixTile _          (Nothing, counter) = (Nothing, incFixTile counter)
fixTile (Tile r c) (Just s, counter)  =
    ( validate $ M.mapWithKey (filterCell $ s M.! Tile r c) s
    , incFixTile counter )
  where
    divr :: Int
    divr = div r 3

    divc :: Int
    divc = div c 3

    dangerTile :: Tile -> Bool
    dangerTile (Tile y x)
      | c == x && r == y    = False
      | c == x || r == y    = True
      | divr == div y 3 && divc == div x 3
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
iterateFixes :: Counter -> Sudoku -> (Maybe Sudoku, Counter)
iterateFixes counter sudoku = foldr fixTile (Just sudoku, counter) (M.keys sudoku)

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
fillSudoku :: Counter -> Sudoku -> (Sudoku, Counter)
fillSudoku counter sudoku = fst $ until noChange nextIteration ((M.empty, undefined), (sudoku, counter))
  where
    noChange (a, b) = ((==) `on` fst) a b
    nextIteration (_, (old, c)) = let (Just s, ncounter) = iterateFixes c old
                                  in  ( (old, incNextIteration ncounter)
                                      , (fixSingles s, incNextIteration ncounter) )

--------------------------------------------------------------------------------
-- Solves the sudoku, but returns Nothing if the sudoku is invalid.
solveSudoku :: Counter -> Sudoku -> (Maybe Sudoku, Counter)
solveSudoku co = go (Tile 0 0) co . return
  where
    go :: Tile -> Counter -> Maybe Sudoku -> (Maybe Sudoku, Counter)
    go _ counter Nothing = (Nothing, incGo counter)
    go (Tile r c) counter (Just s)
      | c >= 9      = go (Tile (r + 1) 0) (incGo counter) $ Just s
      | r >= 9      = (Just s, incGo counter)
      | otherwise   = case s M.! Tile r c of
            E list -> go' (Tile r c) (incGo counter) list (Just s)
            D n    -> go (Tile r $ c + 1) (incGo counter) $ Just s
      where
        go' :: Tile -> Counter -> [Int] -> Maybe Sudoku -> (Maybe Sudoku, Counter)
        go' _ counter [] _               = (Nothing, counter)
        go' tile counter (d:ds) Nothing  = (Nothing, counter)
        go' tile counter (d:ds) (Just s) = 
            let (next, ncounter) = go tile (incFixTile counter) $ fst $ fixTile tile (Just $ M.insert tile (D d) s, counter)
            in  case next of
                Nothing -> go' tile ncounter ds (Just s)
                Just s' -> (Just s', ncounter)

--------------------------------------------------------------------------------
-- The main method. Reads a number of sudoku's from stdin and prints their
-- solutions.
main :: IO ()
main = do
    times <- readLn :: IO Int
    replicateM_ times $ do
        sudoku <- parseSudoku
        let (filled, count) = fillSudoku (0, 0, 0) sudoku
        print $ solveSudoku count $ filled
        _ <- getLine
        putStrLn ""

