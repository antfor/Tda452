module Sudoku where

import Test.QuickCheck

------------------------------------------------------------------------------
{- Lab 3A
   Date: -/-/2020
   Authors:Anton Forsberg
   Lab group: 31
 -}
-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row]
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- helper functions
andMap :: (a -> Bool) -> ([a] -> Bool)
andMap f = and . map f

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
{-
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rs) = and [checkRows rs, checkRow rs, checkCells rs]
  where
    checkRows :: [Row] -> Bool
    checkRows rs = length rs == 9
    checkRow :: [Row] -> Bool
    checkRow rs = and $ map (\r -> length r == 9) rs
    checkCells :: [Row] -> Bool
    checkCells rs = and $ map (\r -> and $ map checkCell r) rs
          where
            checkCell :: Cell -> Bool
            checkCell (Just n) = 0 < n && n < 10
            checkCell _ = True
-}

isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rs) = and [checkRows rs, checkRow rs, checkCells rs]
  where
    checkRows :: [Row] -> Bool
    checkRows rs = checkLength rs
    checkRow :: [Row] -> Bool
    checkRow rs = andMap checkLength rs
    checkCells :: [Row] -> Bool
    checkCells rs = andMap (andMap checkCell) rs
      where
        checkCell :: Cell -> Bool
        checkCell (Just n) = 0 < n && n < 10
        checkCell _ = True
    checkLength :: [a] -> Bool
    checkLength as = length as == 9

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
--isFilled (Sudoku rows) = and $ map (\row -> and $ map ((/=) Nothing) row) rows
isFilled (Sudoku rs) = andMap (andMap ((/=) Nothing)) rs


------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku = undefined

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku = undefined

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = undefined


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = undefined

 -- hint: get to know the QuickCheck function vectorOf

-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = undefined
  -- hint: this definition is simple!

------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock = undefined


-- * D2

blocks :: Sudoku -> [Block]
blocks = undefined

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths = undefined

-- * D3

isOkay :: Sudoku -> Bool
isOkay = undefined


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
