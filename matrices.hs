module Matrices (Matrix,
                setMatrix,
                fixedCell,
                completeLine,
                showMatrix,
                showMatrixPossibilities) where
import Data.Maybe
import Data.List
import Data.Char

data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)
type Row  = [Cell]
type Matrix = [Row]

-- ======Set=====
setMatrix :: Int -> Int -> Matrix
setMatrix 0 _ = []
setMatrix aux n = row : (setMatrix (aux-1) n)
    where
        row = [(Possible [1..n]) | x <- [1..n]]


-- ======Operations=====
fixedCell :: Matrix -> (Int, Int) -> Int-> Matrix
fixedCell m (i,j) value = (take i m) ++
                          [(take j (m!!i)) ++
                          [(Fixed value)] ++
                          (drop (j+1) (m!!i))] ++
                          (drop (i+1) m)

completeLine :: Matrix -> (Bool, Bool) -> Int -> Int -> Matrix
completeLine m _ _ 0 = m
completeLine m cond index aux = completeLine (fixedCell m (i,j) ((length m) - aux +1) ) cond index (aux-1)
--completeLine m (False,) index aux = completeLine (fixedCell m (index, ((length m) - aux)) aux) False index (aux-1)
    where
        i = case cond of
            (True, False) -> (length m) - aux
            (False, True) -> index
            (True, True) -> aux - 1
            (False, False) -> index
        j = case cond of
            (True, False) -> index
            (False, True) -> aux - 1
            (True, True) -> index
            (False, False) -> (length m)- aux

-- ======Show=====

showMatrix :: Matrix -> String
showMatrix = unlines . map (unwords . map showCell)
    where
        showCell (Fixed x) = show x ++ "   "
        showCell _ = "."

showMatrixPossibilities :: Matrix -> String
showMatrixPossibilities = unlines . map (unwords . map showCell)
  where
    showCell (Fixed x)     = show x ++ "    "
    showCell (Possible xs) =
      (++ "]")
      . Data.List.foldl' (\acc x -> acc ++ if x `elem` xs then show x else " ") "["
      $ [1..(length xs)]
