module Main (main) where

import Wolkenkratzer
import Matrices

main = do
    print("=================")
    print("= Wolkenkratzer =")
    print("=================")

    print(myExample 1)
    print(fst (myExample 1))

    let i = 1
    let j = 0

    print(fixedCell (fst (myExample 1)) (i,j) 6)
    putStrLn(showMatrixPossibilities (fixedCell (fst (myExample 1)) (i,j) 8))

    putStrLn(showMatrixPossibilities (fst (pruneWk (myExample 1) 0)))
    putStrLn(showMatrixPossibilities (fst (pruneWk (myExample 2) 0)))
