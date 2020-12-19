{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import System.IO
import qualified Data.Set as S

type Coords = (Int, Int)

type CoordsExt = (Int, Int, Int)

type Instr = (Char, Int)

toInstr:: String -> Instr
toInstr (x:xs) = (x, read xs)

splitToInstr:: String -> [[Instr]]
splitToInstr content= 
  let 
    l = lines content
    lineToTokens x = map (toInstr . T.unpack ) $ T.split (==',') $ T.pack x
  in
    map lineToTokens l

findVisited:: Int -> Coords -> [Instr] -> S.Set CoordsExt -> S.Set CoordsExt
findVisited _ _ [] mem =  mem
findVisited step pos  ((x, 0):xs) mem = findVisited step pos xs mem
findVisited step (x,y) (('L', a):xs) mem = findVisited (step + 1) (x-1,y) (('L', a - 1):xs) $ S.insert (x-1,y, step+1) mem
findVisited step (x,y) (('R', a):xs) mem = findVisited (step + 1) (x+1,y) (('R', a - 1):xs) $ S.insert (x+1,y, step+1) mem
findVisited step (x,y) (('U', a):xs) mem = findVisited (step + 1) (x,y+1) (('U', a - 1):xs) $ S.insert (x,y+1, step+1) mem
findVisited step (x,y) (('D', a):xs) mem = findVisited (step + 1) (x,y-1) (('D', a - 1):xs) $ S.insert (x,y-1, step+1) mem

inLowestStep:: Coords -> S.Set CoordsExt -> Int
inLowestStep (x,y) mem = foldr1 min $ map (\(_,_,c) -> c) $ filter (\(a,b,_) -> a == x && b == y) $ S.toList mem

main :: IO ()
main = do
  h <- openFile "input.txt" ReadMode
  content <- hGetContents h
  let instructions = splitToInstr content
  let a:b:_ = instructions 
  print a
  print b
  let visitedA = findVisited 0 (0,0) a S.empty
  let visitedB = findVisited 0 (0,0) b S.empty
  let inter = S.intersection (S.map (\(x,y,_) -> (x,y)) visitedA) (S.map (\(x,y,_) -> (x,y)) visitedB)
  let res = S.findMin $ S.map (\coords -> inLowestStep coords visitedA + inLowestStep coords visitedB) inter
  print res
  hClose h
