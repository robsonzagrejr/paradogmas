-- ============================
-- === Module Wolkenkratzer ===
-- ============================

module Wolkenkratzer (Wk,
                      pruneWk,
                      pruneWkVs,
                      solverWk,
                      myExample) where

-- ======================
-- === Import Modules ===
-- ======================


-- Matrices: Used to represent the board of game

import Control.Applicative ((<|>))
import Matrices
import Checkup



-- === Visual Skyscraper ===
-- Array of visible skyscrapers in order = top-right-bottom-left
type VisSkyscraper = [[Int]]

type Wk = (Matrix, VisSkyscraper)

-- ================
-- === Examples ===
-- ================

-- === Examples of Boards ===
myExample :: Int -> Wk
myExample 1 = ((setMatrix 3 3), ([[0,2,0],[3,2,0],[2,0,0],[1,0,2]]))
myExample 2 = ((setMatrix 3 3), ([[0,1,0],[3,1,0],[2,1,0],[1,1,2]]))
myExample 3 = ((setMatrix 3 3), ([[0,3,0],[0,0,0],[0,0,0],[0,3,0]]))
myExample 4 = ((setMatrix 3 3), ([[0,0,0],[0,3,0],[0,3,0],[0,0,0]]))
myExample 5 = ((setMatrix 6 6), ([[0,0,0,0,6,0],[0,6,0,0,0,0],[0,6,0,0,0,0],[0,0,0,0,6,0]]))
myExample 6 = ((setMatrix 3 3), ([[3,3,3],[1,1,1],[1,1,1],[1,1,1]]))
myExample 7 = ((setMatrix 3 3), ([[1,1,1],[1,1,1],[1,1,1],[1,1,1]]))
myExample 8 = ((setMatrix 3 3), ([[0,0,0],[0,0,0],[0,0,0],[0,0,2]]))
myExample 9 = ((setMatrix 3 3), ([[0,2,0],[3,2,0],[2,0,0],[1,0,2]]))
myExample 10 = ((setMatrix 4 4), ([[4, 0, 0, 0], [3, 2, 0, 0], [0, 2, 2, 0], [0, 2, 2, 1]]))
myExample 11 = ((setMatrix 4 4), ([[0, 3, 0, 0], [0, 0, 2, 2], [0, 2, 0, 0], [0, 0, 2, 1]]))
myExample 12 = ((setMatrix 5 5), ([[0, 3, 3, 0, 0], [3, 2, 3, 0, 1], [0, 2, 2, 2, 1], [1, 4, 3, 2, 0]]))
myExample 13 = ((setMatrix 5 5), ([[3, 0, 2, 0, 1], [1, 0, 0, 0, 0], [0, 0, 0, 1, 4], [5, 2, 0, 3, 4]]))
myExample 14 = ((setMatrix 5 5), ([[0, 0, 2, 2, 0], [3, 0, 0, 0, 4], [1, 0, 2, 4, 3], [2, 2, 0, 0, 0]]))
myExample 15 = ((setMatrix 6 6), ([[4, 1, 2, 2, 3, 2], [2, 4, 2, 3, 1, 4],[1, 3, 5, 2, 4, 2], [2, 3, 3, 4, 2, 1]]))
myExample 16 = ((setMatrix 6 6), ([[2, 3, 2, 2, 1, 3], [2, 3, 2, 1, 2, 2], [3, 1, 3, 2, 4, 2], [5, 1, 3, 2, 4, 2]]))




-- ===================================
-- === Operations in Wolkenkratzer ===
-- ===================================


-- === Pruning the Wolkenkratzer ===
-- Pass all visualSkyscraper for the function pruneWKVs
pruneWk :: Wk -> Int -> Wk
pruneWk (m, vs) 4 = (m, vs)
pruneWk (m, vs) i = pruneWk ((pruneWkVs m (vs!!i) i), vs) (i+1)


-- === Pruning the wolkenkratzer based on VisualScryscraper ===
-- Pass coordenades for matrix be filed with fixed values based on 1 and n
pruneWkVs :: Matrix -> [Int] -> Int -> Matrix
pruneWkVs m [] _ = m
pruneWkVs m (x:xs) index | x == 1          = pruneWkVs (fixCell m pos (length m)) xs index
                         | x == (length m) = pruneWkVs (completeLine m cond ((length m) - (length (x:xs))) (length m)) xs index
                         | x > 1           = pruneWkVs (cleanExtreme m pos (x-1) cond) xs index
                         | otherwise       = pruneWkVs m xs index
    where
        -- Position in Matrix (i,j)
        pos = case index of
                -----(Position in Row               , Position in Column            )
                0 -> (0                             , ((length m) - (length (x:xs))))
                1 -> (((length m) - (length (x:xs))), (length m) -1                 )
                2 -> ((length m) - 1                , ((length m) - (length (x:xs))))
                3 -> (((length m) - (length (x:xs))), 0                             )
        cond = case index of
                -----(Column, Inverse)
                0 -> (True  , False  )
                1 -> (False , True   )
                2 -> (True  , True   )
                3 -> (False , False  )

-- === Solver Wolkenkratzer ===
-- Solves the puzzle with prune board
solverWk :: Wk -> Maybe Matrix
solverWk wk = solverAuxWk (pruneWk wk 0)

solverAuxWk :: Wk -> Maybe Matrix
solverAuxWk (matrix, vs) = solverAux matrix vs
    where
        solverAux m v | verifyFixed m = if checkIt (makeIntMatrix m) v then
                                          Just m
                                        else
                                          Nothing
                      | verifyEmpty m = Nothing
                      | otherwise     = let (m1, m2) = nextMatrices m
                                        in solverAuxWk (m1,v) <|> solverAuxWk (m2,v)

