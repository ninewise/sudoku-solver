--------------------------------------------------------------------------------
import           Control.Monad (msum, replicateM, replicateM_)
import           Data.Char     (digitToInt, isNumber)
import qualified Data.Map      as M

--------------------------------------------------------------------------------
-- Tile is the location of a cell in the sudoku grid.
data Tile = Tile Int Int
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- A cell in a sudoku might contain a digit.
type Cell = Maybe Int

--------------------------------------------------------------------------------
-- A sudoku maps the 81 tiles on their cells.
type Sudoku = M.Map Tile Cell

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
    parseCell char | isNumber char  = Just $ digitToInt char
                   | otherwise      = Nothing

--------------------------------------------------------------------------------
-- Gives all the possible digits for a cell.
possibilities :: Tile -> Sudoku -> [Int]
possibilities (Tile r c) sudoku = foldr filterm [1..9]
                    [ Tile r' c' | r' <- [0..8] , c' <- [0..8]
                    , r' /= r || c' /= c
                    , r' == r || c' == c || sameSquare r' c' ]
  where
    sameSquare :: Int -> Int -> Bool
    sameSquare r' c' = div r' 3 == div r 3 && div c' 3 == div c 3

    filterm :: Tile -> [Int] -> [Int]
    filterm tile list = case sudoku M.! tile of
        Nothing -> list
        Just n -> filter (/= n) list

--------------------------------------------------------------------------------
-- The branch and bound part.
solveSudoku :: Sudoku -> Maybe Sudoku
solveSudoku = go $ Tile 0 0
  where
    go :: Tile -> Sudoku -> Maybe Sudoku
    go (Tile r c) sudoku
      | c >= 9 = go (Tile (r + 1) 0) sudoku
      | r >= 9 = Just sudoku
      | otherwise = case sudoku M.! Tile r c of
            Just n -> go (Tile r (c + 1)) sudoku
            Nothing -> msum [ go (Tile r (c + 1)) $ M.insert (Tile r c) n sudoku
                            | n <- map Just $ possibilities (Tile r c) sudoku ]

--------------------------------------------------------------------------------
-- The main method.
main :: IO ()
main = do
    times <- readLn :: IO Int
    replicateM_ times $ do
        sudoku <- parseSudoku
        print $ solveSudoku sudoku
        _ <- getLine
        putStrLn ""
