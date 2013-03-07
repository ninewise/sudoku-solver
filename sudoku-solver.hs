--------------------------------------------------------------------------------
import           Control.Monad (replicateM, msum, replicateM_)
import           Data.Char     (isNumber, digitToInt)
import           Data.Maybe    (fromJust)
import           Data.Map      (Map, mapWithKey, fromList, keys, empty)
import qualified Data.Map      as M
import qualified Data.Foldable as F

--------------------------------------------------------------------------------
data Cell = E [Int] | D Int | X
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
fixTile :: Tile -> Sudoku -> Maybe Sudoku
fixTile (Tile r c) s = validate $ mapWithKey (filterCell $ s M.! Tile r c) s
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
iterateFixes :: Sudoku -> Maybe Sudoku
iterateFixes sudoku = F.foldrM fixTile sudoku (keys sudoku)

--------------------------------------------------------------------------------
fixSingles :: Sudoku -> Sudoku
fixSingles = M.map go
  where
    go (E [n]) = D n
    go x       = x

--------------------------------------------------------------------------------
fillSudoku :: Sudoku -> Sudoku
fillSudoku sudoku = fst $ until noChange nextIteration (empty, sudoku)
  where
    noChange (a, b) = a == b
    nextIteration (_, old) = (old, fixSingles $ fromJust $ iterateFixes old)

--------------------------------------------------------------------------------
solveSudoku :: Tile -> Maybe Sudoku -> Maybe Sudoku
solveSudoku _ Nothing = Nothing
solveSudoku (Tile r c) (Just s)
  | c >= 9      = solveSudoku (Tile (r + 1) 0) $ Just s
  | r >= 9      = Just s
  | otherwise   = case s M.! (Tile r c) of
        E list -> let
                    tile = Tile r c
                    test n = solveSudoku (Tile r $ c + 1) $
                                fixTile tile $ M.insert tile (D n) s
                  in msum $ map test list
        D n -> solveSudoku (Tile r $ c + 1) $ Just s

--------------------------------------------------------------------------------
main :: IO ()
main = do
    times <- (readLn :: IO Int)
    replicateM_ times $ do 
        sudoku <- parseSudoku
        print $ solveSudoku (Tile 0 0) (Just sudoku)
        _ <- getLine
        putStrLn ""
    
