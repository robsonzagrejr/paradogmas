module Main (main) where

import Matrices

my_matrix :: Int -> Matrix
my_matrix 1 = (setMatrix 3 3)

main = do
    print("=================")
    print("= Wolkenkratzer =")
    print("=================")

    print(my_matrix 1)

    putStrLn $ showMatrix (my_matrix 1)

    putStrLn $ showMatrixPossibilities (my_matrix 1)

