--------------------------------------------------------------------------------
import           Control.Monad (liftM, msum, replicateM, replicateM_)
import           Data.Char     (digitToInt, isNumber)
import qualified Data.Foldable as F
import           Data.IORef    (IORef, newIORef, modifyIORef, readIORef)
import qualified Data.Map      as M
import           Data.Maybe    (fromJust)

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
fixTile :: IORef Counter -> Tile -> Maybe Sudoku -> IO (Maybe Sudoku)
fixTile counter (Tile r c) Nothing = do
    modifyIORef counter incFixTile
    return Nothing
fixTile counter (Tile r c) (Just s) = do
    modifyIORef counter incFixTile
    return $ validate $ M.mapWithKey (filterCell $ s M.! Tile r c) s
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
iterateFixes :: IORef Counter -> Sudoku -> IO (Maybe Sudoku)
iterateFixes counter sudoku = F.foldrM (fixTile counter) (Just sudoku) (M.keys sudoku)

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
fillSudoku :: IORef Counter -> Sudoku -> IO Sudoku
fillSudoku counter sudoku = (liftM fst) $ untilM noChange nextIteration (M.empty, sudoku)
  where
    noChange (a, b) = a == b
    nextIteration (_, old) = do
        modifyIORef counter incNextIteration
        new <- iterateFixes counter old
        return (old, fixSingles $ fromJust $ new)


-- Loops a monadic function on its own result until a certain condition is met.
untilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
untilM valid next = go
  where
    go current
      | valid current = return current
      | otherwise     = next current >>= go


--------------------------------------------------------------------------------
-- Solves the sudoku, but returns Nothing if the sudoku is invalid.
solveSudoku :: IORef Counter -> Sudoku -> IO (Maybe Sudoku)
solveSudoku counter = go (Tile 0 0) . Just
  where
    go :: Tile -> Maybe Sudoku -> IO (Maybe Sudoku)
    go _ Nothing = modifyIORef counter incGo >>= \_ -> return Nothing
    go (Tile r c) (Just s)
      | c >= 9      = modifyIORef counter incGo >>= \_ -> go (Tile (r + 1) 0) $ Just s
      | r >= 9      = modifyIORef counter incGo >>= \_ -> return $ Just s
      | otherwise   = do
        modifyIORef counter incGo
        case s M.! Tile r c of
            E list -> do
                let tile = Tile r c
                (liftM msum) $ mapM (\n -> (fixTile counter tile $ Just $ M.insert tile (D n) s) >>= go tile) list
            _ -> go (Tile r $ c + 1) $ Just s


--------------------------------------------------------------------------------
-- The main method. Reads a number of sudoku's from stdin and prints their
-- solutions.
main :: IO ()
main = do
    times <- readLn :: IO Int
    replicateM_ times $ do
        counter      <- newIORef (0, 0, 0)
        sudoku       <- parseSudoku
        filledSudoku <- fillSudoku counter sudoku
        solvedSudoku <- solveSudoku counter filledSudoku
        result       <- readIORef counter
        print solvedSudoku
        print result
        _ <- getLine
        putStrLn ""

