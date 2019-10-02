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

pruneWk :: Wk -> Int -> Wk
pruneWk (m, vs) 4 = (m, vs)
pruneWk (m, vs) i = pruneWk ((pruneWkVs m (vs!!i) i), vs) (i+1)

pruneWkVs :: Matrix -> [Int] -> Int -> Matrix
pruneWkVs m [] _ = m
pruneWkVs m (x:xs) index | x == 1 = pruneWkVs (fixedCell m (i,j) (length m)) xs index
                         | x == 3 = pruneWkvs (completeLine m cond ((length m) - (length (x:xs))))
                         | otherwise = pruneWkVs m xs index
    where
        i = case index of
                0 -> 0
                1 -> ((length m) - (length (x:xs)))
                2 -> (length m) - 1
                3 -> ((length m) - (length (x:xs)))
        j = case index of
                0 -> ((length m) - (length (x:xs)))
                1 -> (length m) - 1
                2 -> ((length m) - (length (x:xs)))
                3 -> 0
        cond = if (
