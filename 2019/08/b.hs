import System.IO
import Data.List.Split
import Data.List (sort, transpose)
import Data.Char (isSpace)
import Control.Monad (forM_)

row = 25
tall = 6

solve :: String -> [String]
solve xs = 
    let 
        layers = chunksOf (row * tall) $ takeWhile (not . isSpace) xs
        t = transpose layers
        front = map (head . filter (/= '2')) t
    in chunksOf row (map (\x -> if x == '0' then ' ' else 'X') front)

main =
    do
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let solution = solve contents
        forM_ solution putStrLn
        hClose handle
