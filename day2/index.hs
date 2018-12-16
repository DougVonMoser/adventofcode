import System.IO
import qualified Data.Map.Strict as Map 
import Data.Maybe

main = do
    input <- readFile "input.txt"
    print $ bar  
          $ lines input


bar :: [String] -> String
bar [] = "oh"
bar (x:xs) = 
    case foo x xs of
        Just (v1, v2) ->
            findCommonLetters v1 v2
        Nothing ->
            bar xs

foo :: String -> [String] -> Maybe (String, String)
foo _ [] = Nothing
foo checker (x:xs) = if isOneCharOff checker x 
                     then Just (checker, x)
                     else foo checker xs

findCommonLetters :: String -> String -> String
findCommonLetters x y = 
    catMaybes
    $ zipWith (\c1 c2 -> if c1 == c2 then Just c1 else Nothing) x y


isOneCharOff :: String -> String -> Bool
isOneCharOff x y = 
         (==) 1
         $ foldl (\diffs (c1, c2) -> if c1 /= c2 then diffs + 1 else diffs) 0
         $ zip x y 






mappingFunc :: String -> (Int, Int)
mappingFunc s  = 
      foldl twoTree (0,0)
    $ foldl (\acc x -> Map.insertWith (+) x 1 acc) Map.empty s
      
twoTree :: (Int, Int) -> Int -> (Int, Int)
twoTree (_, threes) 2 = (1, threes)
twoTree (twos, _) 3 = (twos, 1)
twoTree pair _ = pair
