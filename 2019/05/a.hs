{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import System.IO
import qualified Data.Map.Strict as M

splitToNums:: String -> [Integer]
splitToNums content= map (read . T.unpack) . T.split (==',') $ T.pack content

toMap :: [Integer] -> M.Map Integer Integer
toMap xs = M.fromList $ zip [0..] xs 

(!) = (M.!)

toCommand:: String -> String
toCommand x = if length x == 4 then x else toCommand ('0':x)

idx:: Char -> Integer -> M.Map Integer Integer -> Integer
idx '0' pos m = m ! (m ! pos)
idx '1' pos m = m ! pos

solve :: M.Map Integer Integer -> Integer -> [Integer] -> [Integer]
solve m pos res = 
    case toCommand $ show (m ! pos) of
        m2:m1:"01" -> solve (M.insert (m ! (pos + 3)) ((idx m1 (pos + 1) m)  + (idx m2 (pos + 2) m)) m) (pos + 4) res
        m2:m1:"02" -> solve (M.insert (m ! (pos + 3)) ((idx m1 (pos + 1) m)  * (idx m2 (pos + 2) m)) m) (pos + 4) res
        _:m1:"03" -> solve (M.insert (m ! (pos + 1)) 1 m) (pos + 2) res
        _:m1:"04" -> solve m (pos + 2) ((idx m1 (pos + 1) m):res)
        "0099" -> res
        a -> error $ show a
    

main = do
  h <- openFile "input.txt" ReadMode
  content <- hGetContents h
  let nums = splitToNums content
  let values = toMap nums
  let solution = solve values 0 []
  print $ solution !! 0
  hClose h
