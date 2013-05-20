--------------------------------------------------------------------------------
import           Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar,
                                     putMVar, readMVar, takeMVar, forkIO)
import           Control.Monad      (foldM, liftM, replicateM, when)
import           Data.Char          (digitToInt, isNumber)
import qualified Data.Foldable      as F
import           Data.Function      (on)
import           Data.Functor       ((<$>))
import           Data.List          ((\\))
import           Data.Map           (Map, empty)
import qualified Data.Map           as M

--------------------------------------------------------------------------------
-- Cell represents a cell in the sudoku grid. Contains a possibility of digits,
-- a certain digit or is invalid.
data Cell = E [Int] | D Int | X
    deriving (Eq)

instance Show Cell where
    show (E n)  = '-' : concatMap show n
    show (D n)  = show n
    show (X)    = show "X"

--------------------------------------------------------------------------------
-- Tile is the location of a cell in the sudoku grid.
data Tile = Tile { getR :: Int, getC :: Int }
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
    rows' <- replicateM 21 getLine
    mapM_ (\(t, c) -> modifyCell samurai t $ \_ -> return c)
        [ (Tile r c, parseCell col)
        | (r, row) <- zip [0..] rows'
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
    putStr $ linesOf 42 $ concatMap (\d -> [d, ' ']) str
  where
    printMVar :: Maybe (MVar Cell) -> IO Char
    printMVar Nothing = return ' '
    printMVar (Just m) = do
        cell <- readMVar m
        return $ head $ show cell

    linesOf :: Int -> String -> String
    linesOf _ "" = "\n"
    linesOf n s = take n s ++ "\n" ++ linesOf n (drop n s)

printSamurai' :: Samurai -> IO ()
printSamurai' samurai = do
    str <- liftM concat $ mapM printMVar [ getSamurai samurai (Tile r c) | r <- [0..20], c <- [0..20] ]
    putStr $ linesOf 231 str
  where
    printMVar :: Maybe (MVar Cell) -> IO String
    printMVar Nothing = return $ pad 11 ""
    printMVar (Just m) = do
        cell <- readMVar m
        return $ pad 11 $ show cell

    pad :: Int -> String -> String
    pad n str = str ++ replicate (n - length str) ' '

    linesOf :: Int -> String -> String
    linesOf _ "" = "\n"
    linesOf n s = take n s ++ "\n" ++ linesOf n (drop n s)

--------------------------------------------------------------------------------
-- Simple: If there's only one possibility, that's the one. The returned Bool
-- indicates whether one (or multiple) singles where fixed.
fixSingles :: Sudoku -> IO (Sudoku, Bool)
fixSingles sudoku = F.foldrM go (sudoku, False) (M.keys sudoku)
  where
    go :: Tile -> (Sudoku, Bool) -> IO (Sudoku, Bool)
    go tile (_, changedBefore) = do
        changed <- modifyMVar (sudoku M.! tile) $ \cell -> return $ case cell of
            (E [n]) -> (D n, True)
            x       -> (x, False)
        return (sudoku, changed || changedBefore)


-- ========================================================================== --
--
--  Certain solve strategies.
--
--  The section below contains strategies to solve sudoku's, in a human way.
--  It only fills in things you're sure of. All of these return whether they
--  changed anything.
--
-- ========================================================================== --

-- For all tiles, if this tile is certain, remove that digit from the other
-- (clashing) tiles as a possibility.
removeClashes :: Sudoku -> IO Sudoku
removeClashes sudoku = F.foldrM fixTile sudoku (M.keys sudoku)
  where
    fixTile :: Tile -> Sudoku -> IO Sudoku
    fixTile (Tile r c) _ = F.foldrM (filterTile $ Tile r c) sudoku (M.keys sudoku)

    filterTile :: Tile -> Tile -> Sudoku -> IO Sudoku
    filterTile fixed tofix _ = if clashingTiles fixed tofix
        then do
            fixed_cell <- readMVar $ sudoku M.! fixed
            modifyMVar_ (sudoku M.! tofix) (filterCell fixed_cell)
            return sudoku
        else return sudoku

    filterCell :: Cell -> Cell -> IO Cell
    filterCell (D n) (E ds) = return $ E $ filter (/= n) ds
    filterCell _ t          = return t


-- If within a row, column or tile, a digit can only be placed on one location,
-- that's where he'll go. This became superfluous, as it's included in
-- theseCellsAreBelongToUs.
oneOnOne :: Sudoku -> IO Sudoku
oneOnOne sudoku = F.foldrM findSingle sudoku parts
  where
    findSingle :: [Tile] -> Sudoku -> IO Sudoku
    findSingle part _ = F.foldrM go sudoku [1..9]
      where
        go :: Int -> Sudoku -> IO Sudoku
        go n _ = do
            is <- indices n part sudoku
            case is of [i] -> modifyMVar_ (sudoku M.! (part !! i))
                                (\d -> case d of
                                        D x -> return $ D x
                                        E _ -> return $ E [n]
                                        X   -> return X
                                )
                       _ -> return ()
            return sudoku


-- If x digits are limited to x tiles, no other digits can come into these
-- tiles. For example, when tile A on a row contains [1, 2, 3] and tile B on the
-- same row contains [1, 2], and 1 and 2 do not occur outside these tiles, we
-- can scratch the 3.
theseCellsAreBelongToUs :: Sudoku -> IO Sudoku
theseCellsAreBelongToUs sudoku = F.foldrM go sudoku parts
  where
    -- find patterns like 01101 for 3 in [12,123,3,1,23]
    go :: [Tile] -> Sudoku -> IO Sudoku
    go part _ = filterOnLength <$> patterns >>= write
      where
        patterns :: IO (Map [Int] [Int])
        patterns = F.foldlM insertIndices empty [1..9]

        -- Pairs op list of indices and the digits on those indices.
        insertIndices :: Map [Int] [Int] -> Int -> IO (Map [Int] [Int])
        insertIndices m n = do
            is <- indices n part sudoku
            return $ M.alter (Just . (n:) . F.concat) is m

        -- Where the list of indices has the same length as the digits on those
        -- indices.
        filterOnLength :: Map [Int] [Int] -> Map [Int] [Int]
        filterOnLength = M.filterWithKey ((==) `on` length)

        -- Write the digits on the indices.
        write :: Map [Int] [Int] -> IO Sudoku
        write m = do
            F.mapM_ (\(k, v) ->
                        F.mapM_ (\i ->
                            modifyMVar_ (sudoku M.! (part !! i)) $ \cell -> case cell of
                                D d -> return $ D d
                                E _ -> return $ E v
                                X   -> return X
                        ) k
                    ) $ M.toList m
            return sudoku

-- If x cells contain the same x possible digits, these digits can not occur
-- outside these x cells. For example, tile A contains [1, 2], tile B contains
-- [1, 2] and tile C contains [1, 3]. Then we can scratch the 1 from tile C, as
-- writing a one there would leave us short a number to fill tile A and B.
weStandUnited :: Sudoku -> IO Sudoku
weStandUnited sudoku = F.foldlM go sudoku parts
  where
    go :: Sudoku -> [Tile] -> IO Sudoku
    go _ part = filterOnLength <$> (F.foldlM insertPossibilities first $ zip part [0..]) >>= write
      where
        first = M.singleton [] [] :: Map [Int] [Int]

        insertPossibilities :: Map [Int] [Int] -> (Tile, Int) -> IO (Map [Int] [Int])
        insertPossibilities m (tile, i) = do
            cell <- readMVar (sudoku M.! tile)
            let x = case cell of
                    D _ -> m
                    E d -> M.fromListWith merge $ concatMap (\(k, v) -> posses k v i $ merge k d) $ M.toList m
                    X   -> m
            return x

        --
        posses :: [Int] -> [Int] -> Int -> [Int] -> [([Int], [Int])]
        posses k1 is i k2
          | k1 == k2  = [(k1, merge [i] is)]
          | otherwise = [(k1, is), (k2, merge [i] is)]

        -- Where the list of indices has the same length as the digits on those
        -- indices.
        filterOnLength :: Map [Int] [Int] -> Map [Int] [Int]
        filterOnLength = M.filterWithKey ((==) `on` length)

        write :: Map [Int] [Int] -> IO Sudoku
        write = F.foldrM scratch sudoku . M.toList

        scratch :: ([Int], [Int]) -> Sudoku -> IO Sudoku
        scratch (ps, is) _ = F.foldrM (\i _ -> do
                let mvar = (sudoku M.! (part !! i))
                cell <- takeMVar mvar
                putMVar mvar $ case cell of
                    E d -> E $ d \\ ps
                    x   -> x
                return sudoku
            ) sudoku $ [0..8] \\ is


-- If within a block, all possible location of a digit occur on 1 row/column,
-- that digit cannot occur elsewhere in the row/column.
candidateLine :: Sudoku -> IO Sudoku
candidateLine sudoku = F.foldrM go sudoku blocks
  where
    go :: [Tile] -> Sudoku -> IO Sudoku
    go block _ = do
        (h, v) <- rowcolWise block
        mapM_ (scratch (\r -> filter (not . inRow) [Tile r c | c <- [0..8]])) $ M.toList $ toIndexDigits $ onlyOne h
        mapM_ (scratch (\c -> filter (not . inCol) [Tile r c | r <- [0..8]])) $ M.toList $ toIndexDigits $ onlyOne v
        return sudoku
      where
        rbounds = ( minimum $ map getR block, maximum $ map getR block )
        cbounds = ( minimum $ map getC block, maximum $ map getC block )
        inCol (Tile r _) = fst rbounds <= r && r <= snd rbounds
        inRow (Tile _ c) = fst cbounds <= c && c <= snd cbounds

        scratch :: (Int -> [Tile]) -> (Int, [Int]) -> IO ()
        scratch tiles (i, ds) = mapM_
            (\t -> modifyMVar_ (sudoku M.! t)
                    (\cell -> return $ case cell of
                        E d -> E $ d \\ ds
                        x   -> x
                    )
            ) $ tiles i

    
    -- Maps each index on the rows/cols it's on.
    rowcolWise :: [Tile] -> IO (Map Int [Int], Map Int [Int])
    rowcolWise block = F.foldrM
        (\tile (h, v) -> do
            cell <- readMVar (sudoku M.! tile)
            return $ case cell of
                D d  -> (ins tile (getR) h d, ins tile (getC) v d)
                E ds -> (foldl (ins tile $ getR) h ds, foldl (ins tile $ getC) v ds)
                X    -> (h, v)
        ) (empty, empty) block
      where ins t f m d = M.alter (Just . (merge [f t]) . F.concat) d m

    onlyOne :: Map Int [Int] -> Map Int Int
    onlyOne = M.mapMaybe (\x -> if length x == 1 then Just (head x) else Nothing)

    toIndexDigits :: Map Int Int -> Map Int [Int]
    toIndexDigits = (M.fromListWith (merge)) . (map (\(d, i) -> (i, [d]))) . M.toList
        
    

-- ========================================================================== --
--  Aid functions.
-- ========================================================================== --

-- The rows, columns and blocks of a sudoku.
rows, cols, blocks, parts :: [[Tile]]
rows = [ [Tile r c | c <- [0..8]] | r <- [0..8] ]
cols = [ [Tile r c | r <- [0..8]] | c <- [0..8] ]
blocks = [ [Tile
            (3*(r `mod` 3) + (c `div` 3))
            (3*(r `div` 3) + (c `mod` 3))
         | c <- [0..8] ] | r <- [0..8] ]
parts = rows ++ cols ++ blocks


-- Whether or no the two given tiles (assumed to be 9x9 in the same sudoku) lay
-- in the same row, column or block.
clashingTiles :: Tile -> Tile -> Bool
clashingTiles (Tile r c) (Tile y x)
  | c == x && r == y    = False
  | c == x || r == y    = True
  | div c 3 == div x 3 && div r 3 == div y 3
                        = True
  | otherwise           = False


-- Gets the indices of the given digit in these tiles.
indices :: Int -> [Tile] -> Sudoku -> IO [Int]
indices n part sudoku = liftM fst $ F.foldlM equal ([], 0) part
  where
    equal :: ([Int], Int) -> Tile -> IO ([Int], Int)
    equal (is, i) tile = do
        cell <- readMVar $ sudoku M.! tile
        let match = case cell of
                D d -> d == n
                E d -> n `elem` d
                X   -> False
        return $ if match then (i:is, i + 1) else (is, i + 1)


-- Loops a monadic function on its own result until a certain condition is met.
untilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
untilM valid next = go
  where
    go current
      | valid current = return current
      | otherwise     = next current >>= go

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)
  | x < y     = x : (merge xs $ y:ys)
  | y < x     = y : (merge ys $ x:xs)
  | otherwise = x : merge xs ys
merge [] ys   = ys
merge xs []   = xs


--------------------------------------------------------------------------------
-- Main
main :: IO ()
main = do
    initialSamurai <- parseSamurai
    solvedSamurai  <- samuraiLooper initialSamurai
    printSamurai solvedSamurai
  where
    solveSudokuStep :: (Sudoku, Bool) -> IO (Sudoku, Bool)
    solveSudokuStep (sudoku, _) = do
        sudoku' <- removeClashes sudoku >>= weStandUnited >>= theseCellsAreBelongToUs >>= candidateLine
        fixSingles sudoku'

    samuraiLooper :: Samurai -> IO Samurai
    samuraiLooper (Samurai ul ur ll lr ce) = do
        locks <- replicateM 5 $ newMVar True
        let [ullock, urlock, lllock, lrlock, celock] = locks
        _ <- forkIO $ sudokuLooper ullock ul
        _ <- forkIO $ sudokuLooper urlock ur
        _ <- forkIO $ sudokuLooper lllock ll
        _ <- forkIO $ sudokuLooper lrlock lr
        sudokuLooper celock ce
        changes <- mapM takeMVar locks
        if or changes
            then samuraiLooper $ Samurai ul ur ll lr ce
            else return $ Samurai ul ur ll lr ce

    sudokuLooper :: MVar Bool -> Sudoku -> IO ()
    sudokuLooper lock sudoku = do
        _ <- takeMVar lock
        (_, change) <- solveSudokuStep (sudoku, True)
        _ <- untilM (not . snd) solveSudokuStep (sudoku, change)
        putMVar lock change





