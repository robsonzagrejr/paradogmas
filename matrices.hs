-- =======================
-- === Module Matrices ===
-- =======================


module Matrices (Matrix,
                rowExample,
                setMatrix,
                fixCell,
                completeLine,
                cleanPossibles,
                cleanAll,
                getFixeds,
                removeListInCell,
                cleanLine,
                getCordFirstPossible,
                nextMatrices,
                solverMatrixFixed,
                verifyFixed,
                verifyEmpty,
                makeIntMatrix,
                cleanExtreme,
                showMatrix,
                showMatrixPossibilities) where

-- ======================
-- === Import Modules ===
-- ======================


--import Data.Maybe
import Control.Applicative ((<|>))
import qualified Control.Monad
import Data.List
--import Data.Char

-- === Define data and types ===
-- Data that a cell can be, a fixed number, or a array of possibilites
data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)
-- Type of row in matrix
type Row  = [Cell]
-- Define the type of matrix
type Matrix = [Row]

-- ================
-- === Examples ===
-- ================


-- === Row Example ===
-- Example of row, used in tests
rowExample :: Int -> Row
rowExample 1 = [(Fixed 2), (Fixed 3), (Fixed 2), (Fixed 2)]

-- ============================
-- === Operations in Matrix ===
-- ============================


-- === Define Matrix ===
-- Define a matrix of possibles cells based in a 'm' value
-- TODO Acho pq podemos alterar isso para n por m, aux seria o n
setMatrix :: Int -> Int -> Matrix
setMatrix 0 _   = []
setMatrix aux m = row : (setMatrix (aux-1) m)
    where
        row = [(Possible [1..m]) | x <- [1..m]]


-- === Fix Cell ===
-- Fix a value in a cell in possition (i,j)
fixCell :: Matrix -> (Int, Int) -> Int-> Matrix
fixCell m (i,j) value = (take i m)           ++ --Firsts i rows of matrix
                        [(take j (m!!i))     ++ --Firsts j cells of row
                        [(Fixed value)]      ++ --Put value in cell of row
                        (drop (j+1) (m!!i))] ++ --Lasts (j + 1) cells of row
                        (drop (i+1) m)          --Lasts (i + 1) rows of matrix

-- === Complete Line ===
-- Complete a line of matrix based in a condition, a position in row and a aux value
completeLine :: Matrix -> (Bool, Bool) -> Int -> Int -> Matrix
completeLine m _ _ 0          = m
completeLine m cond index aux = completeLine (fixCell m pos ((length m) - aux +1) ) cond index (aux-1)
    where
        pos = case cond of
              --(Column, Invert)    (Index of Row      , Index of Column   )
                (True  , False ) -> (((length m) - aux), index             )
                (False , True  ) -> (index             , (aux - 1)         )
                (True  , True  ) -> ((aux - 1)         , index             )
                (False , False ) -> (index             , ((length m) - aux))

-- === Clean all Matrix ===
-- clean a matrix in rows and columns
cleanAll :: Matrix -> Matrix
cleanAll m = transpose (cleanPossibles (transpose (cleanPossibles m)))

-- === Clean Possibles ===
-- Remove from possibles cells, values that are fixed in row for all matrix
cleanPossibles :: Matrix -> Matrix
cleanPossibles []     = []
cleanPossibles (x:xs) = [cleanLine x (getFixeds x)] ++ (cleanPossibles xs)

-- === Get Fixed Values ===
-- Return a array of fixed values in a row
getFixeds :: Row -> [Int]
getFixeds []     = []
getFixeds (x:xs) = (getFixed  x) ++ (getFixeds xs)
    where
        getFixed (Fixed v) = [v]
        getFixed (Possible v) = []

-- === Clean Line ===
-- Remove values from possibles cell in a especific row
cleanLine :: Row -> [Int] -> Row
cleanLine [] _          = []
cleanLine (c:cs) values = [(removeListInCell c values)] ++ (cleanLine cs values)

-- === Remove List in Cell ===
-- Remove a list of elements in a possible cell
removeListInCell :: Cell -> [Int] -> Cell
removeListInCell (Fixed c) _          = (Fixed c)
removeListInCell (Possible cs) []     = (Possible cs)
removeListInCell (Possible cs) (x:xs) = (removeListInCell (Possible (removeElementInList cs x)) xs)

-- === Remove Element in List ===
-- Remove element in list
removeElementInList :: [Int] -> Int -> [Int]
removeElementInList [] n    = []
removeElementInList (a:b) n | a == n = (removeElementInList b n)
                            | otherwise = [a] ++ (removeElementInList b n)

-- === Verify Fixed ===
-- Verify if a matrix only have fixed cells
verifyFixed :: Matrix -> Bool
verifyFixed []     = True
verifyFixed (x:xs) | verifyFixedRow x = verifyFixed xs
                   | otherwise = False

-- === Verify Fixed Row ===
-- Verify if row only have fixed cells
verifyFixedRow :: Row -> Bool
verifyFixedRow []     = True
verifyFixedRow (c:cs) | verifyCell c = verifyFixedRow cs
                      | otherwise = False
    where
        --Verify if cell is fixed
        verifyCell (Fixed v) = True
        verifyCell (Possible v) = False

-- === Get Cord First Possible ===
-- Get coordenad of first cell in matrix that is possible
getCordFirstPossible :: Matrix -> Int -> (Int,Int)
getCordFirstPossible [] _     = (-1,-1)
getCordFirstPossible (x:xs) i = if (getFirstPossible x 0) /= -1 then
                                    (i,getFirstPossible x 0)
                                else
                                    (getCordFirstPossible xs (i+1))
    where
        -- Get column of first cell in row that is possible
        getFirstPossible :: Row -> Int -> Int
        getFirstPossible [] _     = -1
        getFirstPossible (c:cs) j | isFixed c = j
                                  | otherwise = getFirstPossible cs (j+1)

-- Return if Cell is Fixed or not
isFixed :: Cell -> Bool
isFixed (Fixed _) = False
isFixed _         = True


-- === Next Matrices === ---
-- Builds two next Matrices: one with the first Possible element Fixed, 
-- other with all Possible without the fixed number
nextMatrices :: Matrix -> (Matrix, Matrix)
nextMatrices m = (cleanAll (fixCell m (i,j) v), putElement m (i,j) b)
    where
        (i,j) = getCordFirstPossible m 0
        v = getElement m (i, j)
        b = changeList ((m !! i) !! j)

-- === Get Element === ---
-- Gets the first element from a Possible list
getElement :: Matrix -> (Int, Int) -> Int
getElement m (i,j) = getFirstElement v
    where
        v = ((m !! i) !! j)
        getFirstElement (Possible []) = -666
        getFirstElement (Possible x) = x!!0
        getFirstElement (Fixed x) = x

-- === Change List ===
-- Remove first element from a list
changeList :: Cell -> Cell
changeList (Possible (x:xs)) = (Possible xs)


-- === Put Element ===
-- Generic implementation for fixCell
putElement :: Matrix -> (Int, Int) -> Cell -> Matrix
putElement m (i,j) value = (take i m)           ++ --Firsts i rows of matrix
                           [(take j (m!!i))     ++ --Firsts j cells of row
                           [value]              ++ --Put value in cell of row
                           (drop (j+1) (m!!i))] ++ --Lasts (j + 1) cells of row
                           (drop (i+1) m)          --Lasts (i + 1) rows of matrix



solverMatrixFixed :: Matrix -> Maybe Matrix
solverMatrixFixed matrix = solverAux (cleanAll matrix)
    where
        solverAux m | verifyFixed m = Just m
                    | verifyEmpty m = Nothing
                    | otherwise = let (m1, m2) = nextMatrices m
                                  in solverMatrixFixed m1 <|> solverMatrixFixed m2
-- === Verify Empty ===
-- Verifies if the next first possible is empty
verifyEmpty :: Matrix -> Bool
verifyEmpty m | (i,j) == (-1, -1) = True
              | getElement m (i,j) == -666 = True
              | otherwise = False
            where
                (i,j) = (getCordFirstPossible m 0)

-- === Make Int Matrix ===
-- Casts a type Matrix to [[Int]], in order to verify
makeIntMatrix :: Matrix -> [[Int]]
makeIntMatrix mat = map makeIntRow mat

makeIntRow :: Row -> [Int]
makeIntRow row = map convertFixed row

convertFixed :: Cell -> Int
convertFixed (Fixed num) = num

-- === Clean Extreme===
-- Clean n elements in a possible cell
-- this function will clean n elements in a line, and n-1 last elements in next cell
cleanExtreme :: Matrix -> (Int,Int) -> Int -> (Bool, Bool) -> Matrix
cleanExtreme m _ 1 _ = m
cleanExtreme m (i,j) n cond | isFixed ((m!!i)!!j) = cleanExtreme aux_matrix pos (n-1) cond
                             | otherwise = cleanExtreme m pos (n-1) cond
    where
        aux_matrix = putElement m (i,j) (removeListInCell ((m!!i)!!j) (getPseudoList (length m) n))
        pos = case cond of
              --(Columns, Inverse)    (Index Row, Index Column)
                (True   , False  ) -> (i+1      , j           ) --Next cell will be the next row in same column
                (False  , True   ) -> (i        , j-1         ) --Next cell will be the previous column in same row
                (True   , True   ) -> (i-1      , j           ) --Next cell will be the previous row in same column
                (False  , False  ) -> (i        , j+1         ) --Next cell will be the next column in same row 

-- === Get Aux List ===
-- Return a list of lentgh qt, start from n and descress in next element
getPseudoList :: Int -> Int -> [Int]
getPseudoList _ 0 = []
getPseudoList n qt = [n] ++ getPseudoList (n-1) (qt-1)

-- ============
-- === Show ===
-- ============

showMatrix :: Matrix -> String
showMatrix = unlines . map (unwords . map showCell)
    where
        showCell (Fixed x) = show x ++ "   "
        showCell _ = "."

showMatrixPossibilities :: Matrix -> String
showMatrixPossibilities = unlines . map (unwords . map showCell)
  where
    showCell (Fixed x)     = show x ++ "        "
    showCell (Possible xs) = show xs
      --(++ "]")
      -- . Data.List.foldl' (\acc x -> acc ++ if x `elem` xs then show x else " ") "["
      -- $ [1..(length xs)]
