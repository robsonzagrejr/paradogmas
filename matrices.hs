module Matrices (Matrix) where
import Data.Maybe
import Data.List
import Data.Char

--Aux
type Size = Int --Row Length

data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)
type Row  = [Cell]
type Matrix = [Row] Size

readMatrix :: String -> Maybe Matrix
readMatrix s
  | length s == 9 = traverse (traverse readCell) . Data.List.Split.chunksOf 3 $ s
  | otherwise      = Nothing
  where
    readCell '.' = Just $ Possible [1..3]
    readCell c
      | Data.Char.isDigit c && c > '0' = Just . Fixed . Data.Char.digitToInt $ c
      | otherwise = Nothing
