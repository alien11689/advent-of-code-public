import System.IO

calcFuel :: Int -> Int
calcFuel v = v `quot` 3 - 2

calcFullFuel :: Int -> Int
calcFullFuel v = 
    let 
        cur = calcFuel v
    in
        if v <= 0 || cur <= 0  then 0 else cur + calcFullFuel cur

main =
    do
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let nums = map read $ lines contents
        let sum = foldr (+) 0 $ map calcFullFuel nums
        print sum
        hClose handle
