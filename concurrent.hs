--------------------------------------------------------------------------------
import           Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import           Control.Monad      (replicateM, when, foldM)
import           Data.Char          (digitToInt, isNumber)
import Data.Map (empty)
import qualified Data.Foldable as F
import qualified Data.Map           as M

--------------------------------------------------------------------------------
-- Cell represents a cell in the sudoku grid. Contains a possibility of digits,
-- a certain digit or is invalid.
data Cell = E [Int] | D Int | X
    deriving (Eq)

instance Show Cell where
    show (E _)  = "-"
    show (D n)  = show n
    show (X)    = show "X"

--------------------------------------------------------------------------------
-- Tile is the location of a cell in the sudoku grid.
data Tile = Tile Int Int
    deriving (Eq, Show, Ord)

--------------------------------------------------------------------------------
-- A sudoku maps the 81 tiles on their cells.
type Sudoku = M.Map Tile (MVar Cell)

--------------------------------------------------------------------------------
-- A Samurai combines 5 sudoku's.
data Samurai = Samurai { getUL :: Sudoku
                       , getUR :: Sudoku
                       , getLL :: Sudoku
                       , getLR :: Sudoku
                       , getCE :: Sudoku
                       }

isUL, isUR, isLL, isLR, isCE :: Tile -> Bool
isUL (Tile r c) =  0 <= r && r <  9 &&  0 <= c && c <  9
isUR (Tile r c) =  0 <= r && r <  9 && 12 <= c && c < 21
isLL (Tile r c) = 12 <= r && r < 21 &&  0 <= c && c <  9
isLR (Tile r c) = 12 <= r && r < 21 && 12 <= c && c < 21
isCE (Tile r c) =  6 <= r && r < 15 &&  6 <= c && c < 15

modifyCell :: Samurai -> Tile -> (Cell -> IO Cell) -> IO ()
modifyCell (Samurai ul ur ll lr ce) tile f = do
    let (Tile r c) = tile
    when (isUL tile && not (isCE tile)) $ modifyMVar_ (ul M.! tile) f
    when (isUR tile && not (isCE tile)) $ modifyMVar_ (ur M.! Tile r (c - 12)) f
    when (isLL tile && not (isCE tile)) $ modifyMVar_ (ll M.! Tile (r - 12) c) f
    when (isLR tile && not (isCE tile)) $ modifyMVar_ (lr M.! Tile (r - 12) (c - 12)) f
    when (isCE tile) $ modifyMVar_ (ce M.! Tile (r - 6) (c - 6)) f

putSamurai :: Samurai -> Tile -> MVar Cell -> Samurai
putSamurai (Samurai ul ur ll lr ce) tile mvar =
    let (Tile r c) = tile
        ul' = if isUL tile then M.insert tile mvar ul else ul
        ur' = if isUR tile then M.insert (Tile r (c - 12)) mvar ur else ur
        ll' = if isLL tile then M.insert (Tile (r - 12) c) mvar ll else ll
        lr' = if isLR tile then M.insert (Tile (r - 12) (c - 12)) mvar lr else lr
        ce' = if isCE tile then M.insert (Tile (r - 6) (c - 6)) mvar ce else ce
    in  Samurai ul' ur' ll' lr' ce'

getSamurai :: Samurai -> Tile -> Maybe (MVar Cell)
getSamurai (Samurai ul ur ll lr ce) (Tile r c)
  | isUL (Tile r c) = M.lookup (Tile r c)               ul
  | isUR (Tile r c) = M.lookup (Tile r (c - 12))        ur
  | isLL (Tile r c) = M.lookup (Tile (r - 12) c)        ll
  | isLR (Tile r c) = M.lookup (Tile (r - 12) (c - 12)) lr
  | isCE (Tile r c) = M.lookup (Tile (r - 6) (c - 6))   ce
  | otherwise       = Nothing

emptySamurai :: IO Samurai
emptySamurai = foldM putEmpty (Samurai empty empty empty empty empty)
    [ Tile r c | r <- [0..20] , c <- [0..20]
    , (isUL ||| isUR ||| isLL ||| isLR ||| isCE) (Tile r c)
    ]
  where (|||) :: (Tile -> Bool) -> (Tile -> Bool) -> Tile -> Bool
        (|||) f g tile = f tile || g tile

        putEmpty :: Samurai -> Tile -> IO Samurai
        putEmpty samurai tile = do
            mvar <- newMVar X
            return $ putSamurai samurai tile mvar

--------------------------------------------------------------------------------
-- parse a Samurai from stdin
parseSamurai :: IO Samurai
parseSamurai = do
    samurai <- emptySamurai
    rows <- replicateM 21 getLine
    mapM_ (\(t, c) -> modifyCell samurai t $ \_ -> return c)
        [ (Tile r c, parseCell col)
        | (r, row) <- zip [0..] rows
        , (c, col) <- zip [0..] row ]
    return samurai
  where
    parseCell char | isNumber char  = D $ digitToInt char
                   | otherwise      = E [1..9]

--------------------------------------------------------------------------------
-- Write a Samurai to stdout
printSamurai :: Samurai -> IO ()
printSamurai samurai = do
    str <- mapM printMVar [ getSamurai samurai (Tile r c) | r <- [0..20], c <- [0..20] ]
    putStr $ linesOf 21 str
  where
    printMVar :: Maybe (MVar Cell) -> IO Char
    printMVar Nothing = return ' '
    printMVar (Just m) = do
        cell <- readMVar m
        return $ head $ show cell

    linesOf :: Int -> String -> String
    linesOf _ "" = "\n"
    linesOf n s = take n s ++ "\n" ++ linesOf n (drop n s)
        
-- ========================================================================== --
--
--  Certain solve strategies.
--
--  The section below contains strategies to solve sudoku's, in a human way.
--  It only fills in things you're sure of. All of these return whether they
--  changed anything.
--
-- ========================================================================== --

-- Simple: If there's only one possibility, that's the one.
onePossibility :: Sudoku -> IO Sudoku
onePossibility sudoku = F.foldrM go sudoku (M.keys sudoku)
  where
    go :: Tile -> Sudoku -> IO Sudoku
    go tile sudoku = do
        modifyMVar_ (sudoku M.! tile) $ \cell -> case cell of
            (E [n]) -> return $ D n
            x       -> return x
        return sudoku
    

-- If within a row, column or tile, a digit can only be placed on one location,
-- that's where he'll go.
--noWhereElseToGo :: Sudoku -> IO Sudoku

-- See bookmark.

-- ========================================================================== --
--  Aid functions.
-- ========================================================================== --

-- Whether or no the two given tiles (assumed to be 9x9 in the same sudoku) lay
-- in the same row, column or block.
clashingTiles :: Tile -> Tile -> Bool
clashingTiles (Tile r c) (Tile y x)
  | c == x && r == y    = False
  | c == x || r == y    = True
  | div c 3 == div x 3 && div r 3 == div y 3
                        = True
  | otherwise           = False


-- For all tiles, if this tile is certain, remove that digit from the other
-- (clashing) tiles as a possibility.
removeClashes :: Sudoku -> IO Sudoku
removeClashes sudoku = F.foldrM fixTile sudoku (M.keys sudoku)
  where
    fixTile :: Tile -> Sudoku -> IO Sudoku
    fixTile (Tile r c) sudoku = F.foldrM (filterTile $ Tile r c) sudoku (M.keys sudoku)

    filterTile :: Tile -> Tile -> Sudoku -> IO Sudoku
    filterTile fixed tofix sudoku = if clashingTiles fixed tofix
        then do
            fixed_cell <- readMVar $ sudoku M.! fixed
            modifyMVar_ (sudoku M.! tofix) (filterCell fixed_cell)
            return sudoku
        else return sudoku

    filterCell :: Cell -> Cell -> IO Cell
    filterCell (D n) (E ds) = return $ E $ filter (/= n) ds
    filterCell _ t          = return t


--------------------------------------------------------------------------------
-- Main
main :: IO ()
main = do
    samurai <- parseSamurai
    printSamurai samurai
