module Matrices (Matrix,
                setMatrix,
                showMatrix,
                showMatrixPossibilities) where
import Data.Maybe
import Data.List
import Data.Char

data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)
type Row  = [Cell]
type Matrix = [Row]

setMatrix :: Int -> Int -> Matrix
setMatrix 0 _ = []
setMatrix aux n = row : (setMatrix (aux-1) n)
    where
        row = [(Possible [1..n]) | x <- [1..n]]

showMatrix :: Matrix -> String
showMatrix = unlines . map (unwords . map showCell)
    where
        showCell (Fixed x) = show x ++ "   "
        showCell _ = "."

showMatrixPossibilities :: Matrix -> String
showMatrixPossibilities = unlines . map (unwords . map showCell)
  where
    showCell (Fixed x)     = show x ++ "          "
    showCell (Possible xs) =
      (++ "]")
      . Data.List.foldl' (\acc x -> acc ++ if x `elem` xs then show x else " ") "["
      $ [1..3]
