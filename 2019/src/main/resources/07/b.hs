{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import System.IO
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import Data.List (permutations)

type Instr = M.Map Integer Integer
type Input = S.Seq Integer
type Output = Input
type State = (Instr, Integer, Input, Bool)
type States = [State]

splitToNums:: String -> [Integer]
splitToNums content= map (read . T.unpack) . T.split (==',') $ T.pack content

toMap :: [Integer] -> Instr
toMap xs = M.fromList $ zip [0..] xs 

(!) = (M.!)

toCommand:: String -> String
toCommand x = if length x == 4 then x else toCommand ('0':x)

idx:: Char -> Integer -> Instr -> Integer
idx '0' pos m = m ! (m ! pos)
idx '1' pos m = m ! pos

initialStates :: Instr -> [Integer] -> States
initialStates instr phases = map (\(cur,phase) -> (instr, 0, S.fromList $ if cur == 0 then [phase, 0] else [phase], False) ) $ zip [0..4] phases

solution:: Instr -> Integer
solution instr = maximum $ map (solveProgram 0) $ map (initialStates instr) $ permutations [5..9]

replace :: Integer -> a -> [a] -> [a]
replace pos newVal list = take (fromIntegral pos) list ++ newVal : drop (fromIntegral (pos+1)) list

solveProgram :: Integer -> States -> Integer
solveProgram cur states =
    if and $ map (\(_,_,_,ended) -> ended) states
        then 
            let ((_,_,input, _):_) = states
                i S.:< _ = S.viewl input 
            in i
        else 
            let nextCur =  mod (cur + 1) $ toInteger $ length states
                nextState@(nextInstr,nextPos,nextInput,nextEnded) = states !! (fromIntegral nextCur)
                curState = states !! (fromIntegral cur)
                (newState, output) = solve curState nextInput
                states' = replace cur newState states
                states'' = replace nextCur (nextInstr,nextPos,output,nextEnded) states'
            in solveProgram nextCur states''

solve :: State -> Output -> (State, Output)
solve state@(m,pos,input,ended) output = 
    case toCommand $ show (m ! pos) of
        m2:m1:"01" -> solve ((M.insert (m ! (pos + 3)) ((idx m1 (pos + 1) m)  + (idx m2 (pos + 2) m)) m), (pos + 4), input, ended) output
        m2:m1:"02" -> solve ((M.insert (m ! (pos + 3)) ((idx m1 (pos + 1) m)  * (idx m2 (pos + 2) m)) m), (pos + 4), input, ended) output
        _:m1:"03" -> 
            if S.null input 
                then (state, output)
                else 
                    let i S.:< ti = S.viewl input
                    in solve ((M.insert (m ! (pos + 1)) i m), (pos + 2), ti, ended) output
        _:m1:"04" -> solve (m, (pos + 2), input, ended) (output S.|> (idx m1 (pos + 1) m))
        m2:m1:"05" -> 
            if idx m1 (pos + 1) m /= 0 
            then solve (m, (idx m2 (pos + 2) m), input, ended) output
            else solve (m, (pos + 3), input, ended) output
        m2:m1:"06" -> 
            if idx m1 (pos + 1) m == 0 
            then solve (m, (idx m2 (pos + 2) m), input, ended) output
            else solve (m, (pos + 3), input, ended) output
        m2:m1:"07" -> 
            if idx m1 (pos + 1) m < idx m2 (pos + 2) m 
            then solve ((M.insert (m ! (pos + 3)) 1 m), (pos + 4), input, ended) output
            else solve ((M.insert (m ! (pos + 3)) 0 m), (pos + 4), input, ended) output
        m2:m1:"08" -> 
            if idx m1 (pos + 1) m == idx m2 (pos + 2) m 
            then solve ((M.insert (m ! (pos + 3)) 1 m), (pos + 4), input, ended) output
            else solve ((M.insert (m ! (pos + 3)) 0 m), (pos + 4), input, ended) output
        "0099" -> ((m, pos, input, True), output)
        a -> error $ show (state, output)
    

main = do
  h <- openFile "input.txt" ReadMode
  content <- hGetContents h
  let nums = splitToNums content
  let values = toMap nums
  print $ solution values
  hClose h
