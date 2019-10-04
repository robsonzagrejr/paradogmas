-- module CheckSolution (checkIt) where
-- import Matrices

--test of real fixed matrix
data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)
type Row  = [Cell]
type Matrix = [Row]

createFixedRow :: Int -> Int -> Int -> Row
createFixedRow x y z = [Fixed x, Fixed y, Fixed z]

fixedMatrix :: Int -> Matrix
fixedMatrix 1 = [(createFixedRow 3 2 1),
                    (createFixedRow 1 3 2),
                    (createFixedRow 2 1 3)] 

convertFixed :: Cell -> Int
convertFixed (Fixed num) = num

makeIntRow :: Row -> [Int]
makeIntRow row = map convertFixed row

makeIntMatrix :: Matrix -> [[Int]]
makeIntMatrix mat = map makeIntRow mat

-- uses the last biggest number to get increments of viewCount
auxCountRow :: [Int] -> Int -> Int
auxCountRow [] _ = 0
auxCountRow (head:body) greatest 
    | greatest > head = 0 + auxCountRow body greatest
    | greatest < head = 1 + auxCountRow body head
    | otherwise = 999

-- Receives wkRow, outputs count of visualizations observed
countRow :: [Int] -> Int
countRow wkRow = auxCountRow wkRow 0

-- Compares wkRowCount with expected VisSkyscraperRowValues
validateRow :: [Int] -> Int -> Bool
validateRow _ 0 = True
validateRow wkRow visRowCount = (countRow wkRow) == visRowCount

-- Exclusive function for left view of wk
validateLeft :: [[Int]] -> [Int] -> Bool
validateLeft _ [] = True
validateLeft (current:next) (head:body) 
    = validateRow current head 
        && validateLeft next body 

-- Exclusive function for rigth view of wk
-- Different from left just because it has to invert wk
validateRigth :: [[Int]] -> [Int] -> Bool
validateRigth _ [] = True
validateRigth (current:next) (head:body) 
    = validateRow (invertRow current) head 
        && validateRigth next body 

-- Used to invert the view for wk rigth and bottom
invertRow :: [Int] -> [Int]
invertRow [] = []
invertRow (head:body) = invertRow(body) ++ [head]

-- Top has the same logic as Left, just for reading purposes
validateTop :: [[Int]] -> [Int] -> Bool
validateTop wk vsRow = validateLeft wk vsRow

-- Bot has the same logic as Rigth, just for reading purposes
validateBottom :: [[Int]] -> [Int] -> Bool
validateBottom wk vsRow = validateRigth wk vsRow

-- Validates Left and Rigth
auxHor :: [[Int]] -> [[Int]] -> Bool
auxHor wk vis = 
    validateLeft wk (getLeft(vis)) && 
    validateRigth wk (getRigth(vis))

-- Validates Top and Bottom
-- Receives transposed wk to reuse row functions
auxVer :: [[Int]] -> [[Int]] -> Bool
auxVer wk vis = 
    validateTop wk (getTop(vis)) && 
    validateBottom wk (getBottom(vis))

-- Whole process, receives wk and vs
checkIt :: [[Int]] -> [[Int]] -> Bool
checkIt wk vis = (auxHor wk vis) && (auxVer (transpose wk) vis) 

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

exMatriz :: Int -> [[Int]]
exMatriz 1 = [ [3, 2, 1],
                [1, 3, 2],
                [2, 1, 3] ]
exMatriz 2 = [ [1, 2, 3],
                [3, 1, 2],
                [2, 3, 1] ]

exVis :: Int -> [[Int]]
exVis 1 = [[0, 2, 0],
            [3, 2, 0],
            [2, 0, 0],
            [1, 0, 2] ]
exVis 2 = [[1, 2, 0],
            [3, 2, 0],
            [2, 0, 1],
            [1, 0, 2] ]

-- Isso tudo vai ter que ser enfiado no wolkenkratzer na verdade
getTop :: [[Int]] -> [Int]
getTop vis = vis!!0

getRigth :: [[Int]] -> [Int]
getRigth vis = vis!!1 

getBottom :: [[Int]] -> [Int]
getBottom vis = vis!!2 

getLeft :: [[Int]] -> [Int]
getLeft vis = vis!!3 


main = do
    print (countRow [1,2,3,4,5])
    print (countRow [3,2,1,4,5])
    print (countRow [3,2,1,5,4])

    print (validateRow [1,2,3,4,5] 5)
    print (validateRow [3,2,1,4,5] 3)
    print (validateRow [3,2,1,5,4] 2)
    print (validateRow [3,2,1,5,4] 0)
    print (validateRow [1,2,3,4,5] 1)
    print (validateRow [3,2,1,4,5] 5)
    print (validateRow [3,2,1,5,4] 8)

    print (exMatriz 1)
    print (exVis 1)

    print (validateLeft (exMatriz 1) (getLeft (exVis 1)))
    print (validateRigth (exMatriz 1) (getRigth (exVis 1)))

    print (transpose (exMatriz 1))

    -- top
    print (validateLeft (transpose (exMatriz 1)) (getTop (exVis 1)))
    -- bot
    print (validateRigth (transpose (exMatriz 1)) (getBottom (exVis 1)))

    print (checkIt (exMatriz 1) (exVis 1))
    print (checkIt (exMatriz 2) (exVis 2))

    print (createFixedRow 3 1 2)
    print (fixedMatrix 1)

    print (convertFixed (Fixed 2))
    print (makeIntRow (createFixedRow 3 1 2))
    print (makeIntMatrix (fixedMatrix 1))
