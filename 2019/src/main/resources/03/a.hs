{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import System.IO
import qualified Data.Set as S

type Coords = (Int, Int)

type Path = [Coords]

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

findVisited:: Coords -> [Instr] -> S.Set Coords -> S.Set Coords
findVisited _ [] mem =  mem
findVisited pos ((x, 0):xs) mem = findVisited pos xs mem
findVisited (x,y) (('L', a):xs) mem = findVisited (x-1,y) (('L', a - 1):xs) $ S.insert (x-1,y) mem
findVisited (x,y) (('R', a):xs) mem = findVisited (x+1,y) (('R', a - 1):xs) $ S.insert (x+1,y) mem
findVisited (x,y) (('U', a):xs) mem = findVisited (x,y+1) (('U', a - 1):xs) $ S.insert (x,y+1) mem
findVisited (x,y) (('D', a):xs) mem = findVisited (x,y-1) (('D', a - 1):xs) $ S.insert (x,y-1) mem

main :: IO ()
main = do
  h <- openFile "input.txt" ReadMode
  content <- hGetContents h
  let instructions = splitToInstr content
  let a:b:_ = instructions 
  print a
  print b
  let inter = S.intersection (findVisited (0,0) a S.empty) (findVisited (0,0) b S.empty)
  let res = S.findMin $ S.map (\(x,y) -> abs x + abs y) inter 
  print res
  hClose h
