--------------------------------------------------------------------------------
import           Control.Monad (replicateM)
import           Data.Char     (isNumber, digitToInt)
import           Data.Map      (Map, mapWithKey, (!), fromList, keys, empty)
import qualified Data.Map      as M

--------------------------------------------------------------------------------
data Cell = E [Int] | D Int
    deriving (Eq)

instance Show Cell where
    show (E l)  = "E " ++ concatMap show l
    show (D n)  = show n

--------------------------------------------------------------------------------
data Tile = Tile Int Int
    deriving (Eq, Show, Ord)

--------------------------------------------------------------------------------
type Sudoku = Map Tile Cell

--------------------------------------------------------------------------------
parseSudoku :: IO Sudoku
parseSudoku = do
    lines <- replicateM 9 getLine
    return $ fromList $
        [ (Tile r c, parseCell char)
        | (r, line) <- zip [0..] lines
        , (c, char) <- zip [0..] line ]
  where
    parseCell char | isNumber char  = D $ digitToInt char
                   | otherwise      = E [1..9]

--------------------------------------------------------------------------------
fixTile :: Tile -> Sudoku -> Sudoku
fixTile (Tile r c) sudoku = mapWithKey (filterCell $ sudoku ! Tile r c) sudoku
  where
    filterCell :: Cell -> Tile -> Cell -> Cell
    filterCell (D a) (Tile r' c') (D b)
      | a==b && r==r' && c/=c'  = error $ "Row mismatch: " ++ info
      | a==b && c==c' && r/=r'  = error $ "Col mismatch: " ++ info
      | a==b && div c 3 == div c' 3 && div r 3 == div r' 3 && c/=c' && r/=r'
                                = error $ "Sqr mismatch: " ++ info
      | otherwise               = D b
      where info = "(" ++ show a ++ " " ++ show r ++ " " ++ show c ++ ")"
                ++ "(" ++ show b ++ " " ++ show r'++ " " ++ show c'++ ")"
                ++ show sudoku
    filterCell (D a) (Tile r' c') (E ds)
      | r == r' || c == c' || (div c 3 == div c' 3 && div r 3 == div r' 3)
                                = E $ filter (/= a) ds
      | otherwise               = E ds
    filterCell (E ds) _ c       = c

--------------------------------------------------------------------------------
iterateFixes :: Sudoku -> Sudoku
iterateFixes sudoku = foldr fixTile sudoku (keys sudoku)

--------------------------------------------------------------------------------
fixSingles :: Sudoku -> Sudoku
fixSingles = M.map go
  where
    go (E [n]) = D n
    go x       = x

--------------------------------------------------------------------------------
solveSudoku :: Sudoku -> Sudoku
solveSudoku sudoku = fst $ until noChange nextIteration (empty, sudoku)
  where
    noChange (a, b) = a == b
    nextIteration (_, old) = (old, fixSingles $ iterateFixes old)

--------------------------------------------------------------------------------
main :: IO ()
main = do
    sudoku <- parseSudoku
    putStrLn $ show sudoku
    
