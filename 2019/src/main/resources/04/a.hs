import qualified Data.Map.Strict as M
import qualified Data.Char as C

doesNotDecrease :: String -> Bool
doesNotDecrease (x:[]) = True
doesNotDecrease (x:y:xs) = C.digitToInt x <= C.digitToInt y && doesNotDecrease (y:xs)

buildMap [] m = m
buildMap (x:xs) m = 
    if M.member x m 
        then buildMap xs $ M.adjust succ x m
        else buildMap xs $ M.insert x 1 m

hasRepetition :: String -> Bool
hasRepetition input = not $ null $ filter (\x -> snd x > 1) $ M.toList $ buildMap input M.empty

isPassword:: Int -> Bool
isPassword input = doesNotDecrease (show input) && hasRepetition (show input)

findMatching:: Int
findMatching =  length $ filter isPassword [156218..652527]

main = do
    print (findMatching)

