import qualified Data.List.Split as Split
import Text.Read 
import Data.Maybe
import qualified Data.Map.Strict as Map 
import Debug.Trace

main = do 
    input <- readFile "input.txt"
    print $ foldl (\acc val -> if val > 1 then acc + 1 else acc) 0
          $ foldl (\acc pair -> Map.insertWith (+) pair 1 acc) Map.empty
          $ concat
          $ map plotRects
          $ map parseInput 
          $ lines input

plotRects :: [Int] -> [(Int, Int)]
plotRects [_, _, _, _, 0] = [] 
plotRects [_, x, y, w, h] = (zip [x..(x+w-1)] $ repeat y) ++ plotRects [0, x, y + 1, w, h - 1]

parseInput :: String -> [Int] 
parseInput = catMaybes . (map (\x -> readMaybe x :: Maybe Int)) . (Split.endByOneOf "# ,:x")


