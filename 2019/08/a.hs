import System.IO
import Data.List.Split
import Data.List (sort)
import Data.Char (isSpace)

row = 25
tall = 6

solve :: String -> Int
solve xs = snd $ head $ sort $ map (\xx -> (length ( filter (\x -> x == '0') xx), length (filter (\x -> x == '1') xx) * length (filter (\x -> x == '2') xx))) $ chunksOf (row * tall) $ takeWhile (not . isSpace) xs

main =
    do
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let solution = solve contents
        print solution
        hClose handle
