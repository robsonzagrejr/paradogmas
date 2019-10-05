-- ============================
-- === Module Wolkenkratzer ===
-- ============================

module Wolkenkratzer (Wk,
                      pruneWk,
                      pruneWkVs,
                      myExample) where

-- ======================
-- === Import Modules ===
-- ======================


-- Matrices: Used to represent the board of game
import Matrices

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


-- ===================================
-- === Operations in Wolkenkratzer ===
-- ===================================


-- === Pruning the Wolkenkratzer ===
-- Pass all visualSkyscraper for the function pruneWKVs
pruneWk :: Wk -> Int -> Wk
pruneWk (m, vs) 4 = (m, vs)
pruneWk (m, vs) i = pruneWk ((pruneWkVs m (vs!!i) i), vs) (i+1)


-- === Pruning the wolkenkratzer based on VisualScryscraper ===
--  Pass coordenades for matrix be filed with fixed values based on 1 and n
pruneWkVs :: Matrix -> [Int] -> Int -> Matrix
pruneWkVs m [] _ = m
pruneWkVs m (x:xs) index | x == 1 = pruneWkVs (fixCell m pos (length m)) xs index
                         | x == (length m) = pruneWkVs (completeLine m cond ((length m) - (length (x:xs))) (length m)) xs index
                         | otherwise = pruneWkVs m xs index
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


