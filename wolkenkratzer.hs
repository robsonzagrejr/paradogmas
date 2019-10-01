module Wolkenkratzer (Wk,
                      my_example) where

import Matrices

type VisSkyscraper = [[Int]] -- array of visible skyscrapers top-right-bottom-left

data Wk = Matrix VisSkyscraper

myExample :: Int -> Wk
myExample 1 = (setMatrix 3 3) ([[0,2,0],[3,2,0],[2,0,0],[1,0,2]])

--    print(my_matrix 1)

 --   putStrLn $ showMatrix (my_matrix 1)

   -- putStrLn $ showMatrixPossibilities (my_matrix 1)
