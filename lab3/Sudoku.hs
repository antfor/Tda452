module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe

------------------------------------------------------------------------------
{- Lab 3A
   Date: 25/11/2020
   Authors: Anton Forsberg and Erik Hermansson
   Lab group: 31
 -}
-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row]
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks

allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 (replicate 9 Nothing)

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle

isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rs) = checkLength rs && checkRows rs && checkCells rs
  where
    checkLength :: [a] -> Bool
    checkLength = (9 ==) . length
    checkRows :: [Row] -> Bool
    checkRows = all checkLength
    checkCells :: [Row] -> Bool
    checkCells = all (all checkCell)
      where
        checkCell :: Cell -> Bool
        checkCell (Just n) = 0 < n && n < 10
        checkCell _ = True

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled = all (notElem Nothing) . rows


------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen

printSudoku :: Sudoku -> IO ()
printSudoku = foldMap (putStrLn . concatMap printCell) . rows
    where
      printCell :: Cell -> String
      printCell (Just n) = show n
      printCell _  = "."

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do
  txt <- readFile path
  let sud = Sudoku $ map (map charToCell) (lines txt)
  if (isSudoku sud)
    then
      return sud
    else
      err
  where
    err = error "Not a Sudoku!!!"
    charToCell:: Char -> Cell
    charToCell '.' = Nothing
    charToCell n | elem n ['1'..'9'] = Just $ digitToInt n
                 | otherwise = err

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = frequency [(9,return Nothing) , (1,Just <$> choose (1,9))]


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = Sudoku <$> (vectorOf 9 (vectorOf 9 cell))


 -- hint: get to know the QuickCheck function vectorOf

-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku


  -- hint: this definition is simple!

------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock b =  intersect b (map Just [1..9]) == (nub b \\ [Nothing])

-- * D2

blocks :: Sudoku -> [Block]
blocks (Sudoku sud) = concat [f sud | f <- [addRows, addCols, add3x3s]]
  where
    addRows :: [Row] -> [Block]
    addRows rs = rs
    addCols :: [Row] -> [Block]
    addCols rs = transpose rs
    add3x3s :: [Row] -> [Block]
    add3x3s rs = [ block r c rs | r <- [0,3,6], c <- [0,3,6] ]
      where
        block :: Int -> Int -> [Row] -> Block
        block r c rs = concatMap (take3 r) (take3 c rs)
        take3 :: Int -> [a] -> [a]
        take3 start list = take 3 (drop start list)

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sud = len 27 bs && all (len 9) bs
    where
      len :: Int -> [a] -> Bool
      len n = (n == ) . length
      bs :: [Block]
      bs = blocks sud

-- * D3

isOkay :: Sudoku -> Bool
isOkay = all isOkayBlock . blocks

---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom right  corner
type Pos = (Int,Int)

-- * E1
blanks :: Sudoku -> [Pos]
blanks = concatMap (\(n,r) -> zip [n,n..] (col r)) . zip [0..] . rows
  where
    col :: Row -> [Int]
    col = fst . unzip . filter (isNothing . snd) . zip [0..]

--test :: Sudoku -> [Pos]
--test sud = [ zip $ fst t $ snd t | t <- unzip3 p
--                                 , p <- (zip3 [0..] [0..] row) , not . isNothing . thd3 p
--                                 | row <- rows sud  ]
--                                 where thd3 (_, _, x) = x
prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = length (blanks allBlankSudoku) == 81


-- * E2

(!!=) :: [a] -> (Int,a) -> [a] --todo
[] !!= a = []
(x:xs) !!= (0,y) = y:xs
(x:xs) !!= (i,y) = x:(xs !!= (i-1 , y))
--snd . unzip . map (\x -> if i == fst x then (i,y) else x) . zip [0..] xs
--[ snd $ unzip nl | nl <- zip [0..] xs, ]
--todo
{-
prop_bangBangEquals_correct :: [Int] -> (Int, Int) -> Bool
prop_bangBangEquals_correct ls (i,n) = checkLength && checkValue && revers
  where
    bang = ls !!= (i , n)
    val = (!!) i bang
    checkLength = length ls == length bang
    checkValue  = (not (isNothing val)) && (fromJust val == n)
    revers = (!!=) ((!!=) ls (i,n))  ( i,((!!) i ls)) == ls
-}


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update (Sudoku rows) (r,c) cell = Sudoku $ rows !!= (r, row)
    where
      row :: Row
      row = (!!) rows r !!= (c, cell)

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1
solve :: Sudoku -> Maybe Sudoku
solve s | isSudoku s = listToMaybe $ solve' s (blanks s)


solve' :: Sudoku -> [Pos] -> [Sudoku]
solve' s _ | not $ isOkay s = []
solve' s (p:ps) = concatMap (\s' -> solve' s' ps) (map (update s p) (map Just [1..9]))
solve' s []     = [s]

-- * F2

readAndSolve :: FilePath -> IO ()
readAndSolve fp =do
   s <- (solveAndPrint <$> (readSudoku fp))
   s
  where
    solveAndPrint :: Sudoku -> IO ()
    solveAndPrint sud | isNothing solution = putStrLn "(no solution)"
                      | otherwise = printSudoku $ fromJust solution
                        where
                          solution :: Maybe Sudoku
                          solution = solve sud
-- * F3


-- * F4
