import qualified Data.List.Split as Split
import Text.Read 
import Data.Maybe
import qualified Data.Map.Strict as Map 
import Debug.Trace

main = do 
    input <- readFile "test.txt"
    print 
          $ (\x ->  let bigassmap = foldl (\acc pair -> Map.insertWith (+) pair 1 acc) Map.empty $ concat x 
                    in finishHim 1 bigassmap x)
          $ map plotRects
          $ map parseInput 
          $ lines input

type Point = (Int, Int)

finishHim :: Int -> Map.Map Point Int -> [[Point]] -> Int
finishHim idx bigassmap (x:xs) = 
    if allYallOnlyOnes x bigassmap then
       idx 
    else 
        finishHim (idx + 1) bigassmap xs

allYallOnlyOnes :: [Point] -> Map.Map Point Int -> Bool
allYallOnlyOnes [] _ = True
allYallOnlyOnes (x:xs) bigassmap = 
    case Map.lookup x bigassmap of 
        Just 1 -> allYallOnlyOnes xs bigassmap
        _ -> False

plotRects :: [Int] -> [(Int, Int)]
plotRects [_, _, _, _, 0] = [] 
plotRects [_, x, y, w, h] = (zip [x..(x+w-1)] $ repeat y) ++ plotRects [0, x, y + 1, w, h - 1]

parseInput :: String -> [Int] 
parseInput = catMaybes . (map (\x -> readMaybe x :: Maybe Int)) . (Split.endByOneOf "# ,:x")


