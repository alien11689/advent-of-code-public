{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import System.IO
import qualified Data.Map.Strict as M

splitToNums:: String -> [Integer]
splitToNums content= map (read . T.unpack) . T.split (==',') $ T.pack content

toMap :: [Integer] -> M.Map Integer Integer
toMap xs = M.fromList $ zip [0..] xs 

(!) = (M.!)

solve :: M.Map Integer Integer -> Integer -> Integer
solve m pos = 
    case m ! pos of
        1 -> solve (M.insert (m ! (pos + 3)) (m ! (m ! (pos + 1)) + m ! (m ! (pos + 2)))  m) (pos + 4)
        2 -> solve (M.insert (m ! (pos + 3)) (m ! (m ! (pos + 1)) * m ! (m ! (pos + 2)))  m) (pos + 4)
        99 -> m ! 0
    

main = do
  h <- openFile "input.txt" ReadMode
  content <- hGetContents h
  let nums = splitToNums content
  let values = toMap nums
  let m = M.insert 1 12 $ M.insert 2 2 values
  let solution = solve m 0
  print solution
  hClose h
