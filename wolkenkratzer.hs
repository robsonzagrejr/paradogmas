module Wolkenkratzer (Wk,
                      pruneWk,
                      pruneWkVs,
                      myExample) where

import Matrices

type VisSkyscraper = [[Int]] -- array of visible skyscrapers top-right-bottom-left

type Wk = (Matrix, VisSkyscraper)

--Examples of Boards
myExample :: Int -> Wk
myExample 1 = ((setMatrix 3 3), ([[0,2,0],[3,2,0],[2,0,0],[1,0,2]]))
myExample 2 = ((setMatrix 3 3), ([[0,1,0],[3,1,0],[2,1,0],[1,1,2]]))
myExample 3 = ((setMatrix 3 3), ([[0,3,0],[0,0,0],[0,0,0],[0,3,0]]))
myExample 4 = ((setMatrix 3 3), ([[0,0,0],[0,3,0],[0,3,0],[0,0,0]]))
myExample 5 = ((setMatrix 6 6), ([[0,0,0,0,6,0],[0,6,0,0,0,0],[0,6,0,0,0,0],[0,0,0,0,6,0]]))
myExample 6 = ((setMatrix 3 3), ([[3,3,3],[1,1,1],[1,1,1],[1,1,1]]))
myExample 7 = ((setMatrix 3 3), ([[1,1,1],[1,1,1],[1,1,1],[1,1,1]]))

pruneWk :: Wk -> Int -> Wk
pruneWk (m, vs) 4 = (m, vs)
pruneWk (m, vs) i = pruneWk ((pruneWkVs m (vs!!i) i), vs) (i+1)

pruneWkVs :: Matrix -> [Int] -> Int -> Matrix
pruneWkVs m [] _ = m
pruneWkVs m (x:xs) index | x == 1 = pruneWkVs (fixCell m pos (length m)) xs index
                         | x == (length m) = pruneWkVs (completeLine m cond ((length m) - (length (x:xs))) (length m)) xs index
                         | otherwise = pruneWkVs m xs index
    where
        pos = case index of
                0 -> (0, ((length m) - (length (x:xs))))
                1 -> (((length m) - (length (x:xs))), (length m) -1)
                2 -> ((length m) - 1, ((length m) - (length (x:xs))))
                3 -> (((length m) - (length (x:xs))), 0)
        cond = case index of
                -----(Column, Inverse)
                0 -> (True, False)
                1 -> (False, True)
                2 -> (True, True)
                3 -> (False, False)


