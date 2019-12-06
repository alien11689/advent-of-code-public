{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import System.IO
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Dir = (String, String)

toDir:: String -> Dir
toDir s = (takeWhile (/= ')') s, tail $ dropWhile (/= ')') s)

countOrbits:: [Dir] -> Int
countOrbits dirs = countOrbits' dirs ["COM"] $ M.insert "COM" 0 M.empty 

addToMap [] _ m = m
addToMap (x:xs) c m = addToMap xs c $ M.insert x c m

(!) = (M.!)

countOrbits' :: [Dir] -> [String] -> M.Map String Int -> Int
countOrbits' _ [] m = sum $ map snd $ M.toList m
countOrbits' dirs (cur:xs) m = 
    let dests = map snd $ filter (\(a,_) -> a == cur) dirs
        m' = addToMap dests (m ! cur + 1) m
    in  countOrbits' dirs (dests ++ xs) m'

main = do
  h <- openFile "input.txt" ReadMode
  content <- hGetContents h
  let dirs = map toDir $ lines content
  let res = countOrbits dirs
  print res
  hClose h
