import System.IO

calcFuel :: Int -> Int
calcFuel v = v `quot` 3 - 2

main =
    do
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let nums = map read $ lines contents
        let sum = foldr (+) 0 $ map calcFuel nums
        print sum
        hClose handle
