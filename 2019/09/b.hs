{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import System.IO
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S

type Instr = M.Map Integer Integer
type Input = S.Seq Integer
type Output = Input
type State = (Instr, Integer, Input, Bool, Integer)
type States = [State]

splitToNums:: String -> [Integer]
splitToNums content= map (read . T.unpack) . T.split (==',') $ T.pack content

toMap :: [Integer] -> Instr
toMap xs = M.fromList $ zip [0..] xs 

m ! k = M.findWithDefault 0 k m

toCommand:: String -> String
toCommand x = if length x == 5 then x else toCommand ('0':x)

idx:: Char -> Integer -> Integer -> Instr -> Integer
idx '0' pos rel m = m ! (m ! pos)
idx '1' pos rel m = m ! pos
idx '2' pos rel m = m ! (rel + (m ! pos))

solution:: Instr -> Output
solution instr = snd $ solve (instr, 0, S.fromList [2], False, 0) (S.empty)

assignTo:: Instr -> Integer -> Char -> Integer -> Integer -> Instr
assignTo m pos '0' _ value = M.insert (m ! pos) value m
assignTo m pos '2' rel value = M.insert (rel + (m ! pos)) value m

solve :: State -> Output -> (State, Output)
solve state@(m,pos,input,ended,rel) output =
    case toCommand $ show (m ! pos) of
        m3:m2:m1:"01" -> solve (assignTo m (pos + 3) m3 rel ((idx m1 (pos + 1) rel m)  + (idx m2 (pos + 2) rel m)), (pos + 4), input, ended, rel) output
        m3:m2:m1:"02" -> solve (assignTo m (pos + 3) m3 rel ((idx m1 (pos + 1) rel m)  * (idx m2 (pos + 2) rel m)), (pos + 4), input, ended, rel) output
        _:_:m1:"03" ->
            if S.null input
                then (state, output)
                else
                    let i S.:< ti = S.viewl input
                    in solve (assignTo m (pos + 1) m1 rel i, (pos + 2), ti, ended, rel) output
        _:_:m1:"04" -> solve (m, (pos + 2), input, ended, rel) (output S.|> (idx m1 (pos + 1) rel m))
        m3:m2:m1:"05" ->
            if idx m1 (pos + 1) rel m /= 0
            then solve (m, (idx m2 (pos + 2) rel m), input, ended, rel) output
            else solve (m, (pos + 3), input, ended, rel) output
        m3:m2:m1:"06" ->
            if idx m1 (pos + 1) rel m == 0
            then solve (m, (idx m2 (pos + 2) rel m), input, ended, rel) output
            else solve (m, (pos + 3), input, ended, rel) output
        m3:m2:m1:"07" ->
            if idx m1 (pos + 1) rel m < idx m2 (pos + 2) rel m
            then solve (assignTo m (pos + 3) m3 rel 1, (pos + 4), input, ended, rel) output
            else solve (assignTo m (pos + 3) m3 rel 0, (pos + 4), input, ended, rel) output
        m3:m2:m1:"08" ->
            if idx m1 (pos + 1) rel m == idx m2 (pos + 2) rel m
            then solve (assignTo m (pos + 3) m3 rel 1, (pos + 4), input, ended, rel) output
            else solve (assignTo m (pos + 3) m3 rel 0, (pos + 4), input, ended, rel) output
        _:_:m1:"09" ->
            solve (m, (pos + 2), input, ended, rel + (idx m1 (pos + 1) rel m)) output
        "00099" -> ((m, pos, input, True, rel), output)
        a -> error $ show a
    

main = do
  h <- openFile "input.txt" ReadMode
  content <- hGetContents h
  let nums = splitToNums content
  let values = toMap nums
  print $ solution values
  hClose h
