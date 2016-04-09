module SodukuUppgift where

import Data.Char
import Test.QuickCheck
import Data.List
import Data.Maybe
import System.IO

data Sudoku = Sudoku [[Maybe Int]] deriving Show

rows :: Sudoku -> [[Maybe Int]] 
rows (Sudoku rs) = rs

{-| Assignment A  -}
-- Will give you a completely blank Soduko
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [allBlankRow | x <- [1..9]]

-- Will give you a completely blank Soduko row
allBlankRow :: [Maybe Int]
allBlankRow = [Nothing | x <- [20..28]]

isSudoku :: Sudoku -> Bool
isSudoku sud = isSudokuValuesRow (rows sud) && (length (rows sud) == 9)

isSudokuValuesRow :: [[Maybe Int]] -> Bool
isSudokuValuesRow [] = True
isSudokuValuesRow (ro:ros) = 
 and (isSudokuValues ro) && length ro == 9 && isSudokuValuesRow ros

isSudokuValues :: (Num a, Ord a) => [Maybe a] -> [Bool]
isSudokuValues []   = [False]
isSudokuValues list = [True | x <- list, valueOfMaybe x]
 where valueOfMaybe Nothing = True
       valueOfMaybe x = resultMaybe x > 0 && resultMaybe x < 10
         where resultMaybe (Just x) = x 

-- isSolved sud checks if sud is already solved, i.e. No "Nothing"s
isSolved :: Sudoku -> Bool
isSolved sud = isNumValuesRow (rows sud)
 where isNumValuesRow = foldr ((&&) . and . isNumValues) True
       isNumValues list = [isNothing x | x <- list]

{-| Assignment B1 -}
printSudoku :: Sudoku -> IO ()
printSudoku sud = printSudHelper (rows sud)

printSudHelper :: [[Maybe Int]] -> IO ()
printSudHelper [] = return ()
printSudHelper (ro:ros) = do printSudRow ro
                             printSudHelper ros

printSudRow :: [Maybe Int] -> IO ()
printSudRow [] = putStrLn ""
printSudRow (ro:ros) = do putChar $ parseMaybeToStr ro
                          printSudRow ros
                          where parseMaybeToStr Nothing  = '.'
                                parseMaybeToStr (Just v) = intToDigit v

{-| Assignment B2 -}
readSudoku :: FilePath -> IO Sudoku
readSudoku path = 
 do 
  handle  <- openFile path ReadMode
  content <- hGetContents handle
  let ros = makeSudoku $ lines content
  return $ if isSudoku ros then ros else error "Not a Sudoku"


makeSudoku :: [String] -> Sudoku
makeSudoku str = 
 Sudoku [[(\y -> if y == '.' 
                 then Nothing 
                 else Just (digitToInt y)) y | y <- x] | x <- str]
                                 
{-| Assignment C1 -}
cell :: Gen (Maybe Int)
cell = frequency [ (90, return Nothing),
                   (10, do r <- choose(1,9)
                           return (Just r))]

{-| Assignment C2 -} 
instance Arbitrary Sudoku where
 arbitrary = 
  do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
     return (Sudoku rows)
                
{-| Assignment C3 -} 
prop_sudoku :: Sudoku -> Bool
prop_sudoku = isSudoku