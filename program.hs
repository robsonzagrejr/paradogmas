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

    print(fixCell (fst (myExample 1)) (i,j) 6)
    putStrLn(showMatrixPossibilities (fixCell (fst (myExample 1)) (i,j) 8))

    putStrLn(showMatrixPossibilities (fst (pruneWk (myExample 1) 0)))
    putStrLn(showMatrixPossibilities (fst (pruneWk (myExample 2) 0)))
    putStrLn(showMatrixPossibilities (fst (pruneWk (myExample 3) 0)))
    putStrLn(showMatrixPossibilities (fst (pruneWk (myExample 4) 0)))
    putStrLn(showMatrixPossibilities (fst (pruneWk (myExample 5) 0)))


    --putStrLn(showMatrixPossibilities (cleanProssibles (fst (pruneWk (myExample 5) 0))))
    print(rowExample 1)
    print(getFixeds (rowExample 1))
    print(cleanLine (rowExample 1) (getFixeds (rowExample 1)))

    print("========Normal========")
    putStrLn(showMatrixPossibilities (fst (pruneWk (myExample 5) 0)))
    print("========Clean========")
    putStrLn(showMatrixPossibilities (cleanPossibles (fst (pruneWk (myExample 5) 0))))
    print(cleanPossibles (fst (pruneWk (myExample 5) 0)))
    print(apagarLista (apagarLista ((rowExample 1)!!0) [3,4]) [5])
    print("===========Normal==========")
    putStrLn(showMatrixPossibilities (cleanPossibles (fst (pruneWk (myExample 5) 0))))
    print("==========Transposta======")
    --putStrLn(showMatrixPossibilities (transpose (cleanPossibles (fst (pruneWk (myExample 5) 0)))))

    print("==========ALLLL======")
    putStrLn(showMatrixPossibilities (cleanAll (fst (pruneWk (myExample 5) 0))))
    print("==========VerifyFixed======")
    print(verifyFixed (fst (pruneWk (myExample 6) 0)))
    print(verifyFixed (fst (pruneWk (myExample 5) 0)))
    print(getCordFirstPossible (fst (pruneWk (myExample 7) 0)) 0)
