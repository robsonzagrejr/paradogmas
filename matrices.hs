module Matrices (Matrix,
                rowExample,
                setMatrix,
                fixCell,
                completeLine,
                cleanPossibles,
                cleanAll,
                getFixeds,
                apagarLista,
                cleanLine,
                verifyFixed,
                getCordFirstPossible,
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
fixCell :: Matrix -> (Int, Int) -> Int-> Matrix
fixCell m (i,j) value = (take i m)           ++ --Firsts i rows of matrix
                          [(take j (m!!i))     ++ --Firsts j cells of row
                          [(Fixed value)]      ++ --Put value in cell of row
                          (drop (j+1) (m!!i))] ++ --Lasts (j + 1) cells of row
                          (drop (i+1) m)          --Lasts (i + 1) rows of matrix

completeLine :: Matrix -> (Bool, Bool) -> Int -> Int -> Matrix
completeLine m _ _ 0 = m
completeLine m cond index aux = completeLine (fixCell m pos ((length m) - aux +1) ) cond index (aux-1)
    where
        pos = case cond of
            ----(Column, Invert)   (Index of Row      , Index of Column )
                (True  , False) -> (((length m) - aux), index           )
                (False , True)  -> (index             , (aux - 1)       )
                (True  , True)  -> ((aux - 1)         , index           )
                (False , False) -> (index             , ((length m) - aux))

cleanAll :: Matrix -> Matrix
cleanAll m = transpose (cleanPossibles (transpose (cleanPossibles m)))

cleanPossibles :: Matrix -> Matrix
cleanPossibles [] = []
cleanPossibles (x:xs) = [cleanLine x (getFixeds x)] ++ (cleanPossibles xs)

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


verifyFixed :: Matrix -> Bool
verifyFixed [] = True
verifyFixed (x:xs) | verifyFixedRow x = verifyFixed xs
                        | otherwise = False

verifyFixedRow :: Row -> Bool
verifyFixedRow [] = True
verifyFixedRow (c:cs) | verifyCell c = verifyFixedRow cs
                      | otherwise = False
    where
        verifyCell (Fixed v) = True
        verifyCell (Possible v) = False

getCordFirstPossible :: Matrix -> Int -> (Int,Int)
getCordFirstPossible [] _ =(-1,-1)
getCordFirstPossible (x:xs) i = if (getFirstPossible x 0) /= -1 then
                        (i,getFirstPossible x 0)
                   else
                        (getCordFirstPossible xs (i+1))
    where
        getFirstPossible :: Row -> Int -> Int
        getFirstPossible [] _ = -1
        getFirstPossible (c:cs) j | isFixed c = j
                                  | otherwise = getFirstPossible cs (j+1)
            where
                isFixed (Fixed _) = False
                isFixed _ = True


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
