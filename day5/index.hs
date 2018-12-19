import qualified Data.Map.Strict as Map
import Debug.Trace

main =  do
    input <- readFile "input.txt"
    print  
            $ length $ runner
            $ unwords $ words input 

doer :: String -> String
doer = foldl folder [] 

runner :: String -> String
runner x =
    runHelp x $ length x 
    
runHelp :: String -> Int -> String 
runHelp hunk len = 
    let x = doer hunk
        xlen = length x 
    in 
        if xlen == len
        then x
        else runHelp x xlen

pairConverter :: Map.Map Char Char
pairConverter = 
    Map.fromList $ zip ['A'..'Z'] ['a'..'z'] ++ zip ['a'..'z'] ['A'..'Z'] 

folder :: String -> Char -> String
folder [] currChar = [currChar]
folder hunk currChar = 
    if Map.lookup ( last hunk)  pairConverter == Just currChar 
    then reverse $ tail $ reverse hunk
    else hunk ++ [currChar] 
 
