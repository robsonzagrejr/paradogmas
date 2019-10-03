module Matrices (Matrix,
                rowExample,
                setMatrix,
                fixedCell,
                completeLine,
                cleanPossibles,
                getFixeds,
                apagarLista,
                cleanLine,
                showMatrix,
                showMatrixPossibilities) where
--import Data.Maybe
import Data.List
--import Data.Char

data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)
type Row  = [Cell]
type Matrix = [Row]

rowExample :: Int -> Row
rowExample 1 = [(Possible [1..7]), (Fixed 3), (Fixed 2), (Possible [1..5])]

-- ======Set=====
setMatrix :: Int -> Int -> Matrix
setMatrix 0 _ = []
setMatrix aux n = row : (setMatrix (aux-1) n)
    where
        row = [(Possible [1..n]) | x <- [1..n]]

-- ======Operations=====
fixedCell :: Matrix -> (Int, Int) -> Int-> Matrix
fixedCell m (i,j) value = (take i m)           ++ --Firsts i rows of matrix
                          [(take j (m!!i))     ++ --Firsts j cells of row
                          [(Fixed value)]      ++ --Put value in cell of row
                          (drop (j+1) (m!!i))] ++ --Lasts (j + 1) cells of row
                          (drop (i+1) m)          --Lasts (i + 1) rows of matrix

completeLine :: Matrix -> (Bool, Bool) -> Int -> Int -> Matrix
completeLine m _ _ 0 = m
completeLine m cond index aux = completeLine (fixedCell m pos ((length m) - aux +1) ) cond index (aux-1)
    where
        pos = case cond of
            ----(Column, Invert)   (Index of Row      , Index of Column )
                (True  , False) -> (((length m) - aux), index           )
                (False , True)  -> (index             , (aux - 1)       )
                (True  , True)  -> ((aux - 1)         , index           )
                (False , False) -> (index             , ((length m) - aux))

cleanPossibles :: Matrix -> Matrix
cleanPossibles [] = []
cleanPossibles (x:xs) =  [cleanLine x (getFixeds x)] ++ (cleanPossibles xs)

getFixeds :: Row -> [Int]
getFixeds [] = []
getFixeds (x:xs) = (getFixed  x) ++ (getFixeds xs)
    where
        getFixed (Fixed v) = [v]
        getFixed (Possible v) = []

cleanLine :: Row -> [Int] -> Row
cleanLine [] _ = []
cleanLine (c:cs) values = [(apagarLista c values)] ++ (cleanLine cs values)

apagar :: [Int] -> Int -> [Int]
apagar [] n= []
apagar (a:b) n | a == n = (apagar b n)
               | otherwise = [a] ++ (apagar b n)

apagarLista :: Cell -> [Int] -> Cell
apagarLista (Fixed c) _ = (Fixed c)
apagarLista (Possible cs) [] = (Possible cs)
apagarLista (Possible cs) (x:xs) = (apagarLista (Possible (apagar cs x)) xs)

-- ======Show=====

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
