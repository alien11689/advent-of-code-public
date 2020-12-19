{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import System.IO
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Dir = (String, String)

toDir:: String -> Dir
toDir s = (takeWhile (/= ')') s, tail $ dropWhile (/= ')') s)

countOrbits:: [Dir] -> Int
countOrbits dirs = countOrbits' dirs ["YOU"] $ M.insert "YOU" 0 M.empty 

addToMap [] _ m = m
addToMap (x:xs) c m = addToMap xs c $ M.insert x c m

(!) = (M.!)

countOrbits' :: [Dir] -> [String] -> M.Map String Int -> Int
countOrbits' _ [] m = m ! "SAN" - 2 
countOrbits' dirs (cur:xs) m = 
    let destsA = map snd $ filter (\(a,_) -> a == cur) dirs
        destsB = map fst $ filter (\(_,a) -> a == cur) dirs
        dests = filter (\x -> not $ M.member x m) (destsA ++ destsB)
        m' = addToMap dests (m ! cur + 1) m
    in  countOrbits' dirs (dests ++ xs) m'

main = do
  h <- openFile "input.txt" ReadMode
  content <- hGetContents h
  let dirs = map toDir $ lines content
  let res = countOrbits dirs
  print res
  hClose h
