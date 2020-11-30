module Sudoku where
import Test.QuickCheck
import Control.Monad
import Data.Maybe
import Data.Char
import Data.List
------------------------------------------------------------------------------

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
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = length (sRows) == 9 && and [ and [ isNothing c || (fromJust c > 0 && fromJust c < 10) | c <- sRow] && length sRow == 9 | sRow <-sRows]
      where sRows = rows s

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled s = and [and [ isJust c | c <- sRow] | sRow <- sRows]
      where sRows = rows s

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = putStr (unlines [ [ cellToChar c | c <- sRow] | sRow <-sRows])
          where sRows = rows s

-- | charToCell converts a Cell to a Char
cellToChar :: Cell -> Char
cellToChar c | isNothing c = '.'
cellToChar c = intToDigit (fromJust c)

-- * B2
-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
                fc <- readFile fp
                if isSudoku((Sudoku ([[charToCell c |c <- row] | row <- lines fc])))
                  then return (Sudoku ([[charToCell c |c <- row] | row <- lines fc]))
                  else error "not valid sudoku"

-- | charToCell converts a Char to a cell
charToCell :: Char -> Cell
charToCell c | c == '.' = Nothing
charToCell c = Just $ digitToInt c

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen Cell
cell = frequency [(9, return Nothing), (1, Just <$> choose(1,9))]

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do
          x <- (vectorOf 9 (vectorOf 9 cell))
          return (Sudoku x)

 -- hint: get to know the QuickCheck function vectorOf

-- * C3
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
  -- hint: this definition is simple!

------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1
-- | isOkayBlock checks if Block contains the same digit twice
isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (c:cs) = (isNothing c || notElem c cs) && (isOkayBlock cs)

-- * D2
-- | blocks splits a sodoku into 9 3x3 blocks
blocks :: Sudoku -> [Block]
blocks s = [[sRows !! (i + 3 * div k 3) !! (j + mod (3 * k) 9) | i <- [0..2], j <- [0..2]] | k <- [0..8]]
          where sRows = rows s

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths s = length sBlocks == 9 && and [and [isJust c || isNothing c | c <- block] && length block == 9 | block <- sBlocks]
                    where sBlocks = blocks s

-- * D3
-- | isOkay checks that there are no repeat digits in each block, row and column of Sudoku
isOkay :: Sudoku -> Bool
isOkay s = and [isOkayBlock block | block <- blocks s] && and [isOkayBlock block | block <- sRows] && and [isOkayBlock block | block <- transpose sRows]
        where sRows = rows s


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
