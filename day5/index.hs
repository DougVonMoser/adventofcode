import qualified Data.Map.Strict as Map
import Debug.Trace
import Data.Char

main =  do
    input <- readFile "input.txt"
    print  
            $ humbug 
            $ unwords $ words input 


humbug hunk = 
    let maxInt = maxBound :: Int 
    in
        foldl (humFolder hunk) maxInt ['a'..'z'] 

humFolder :: String -> Int -> Char -> Int
humFolder hunk winningCount currChar =
    let tester = toUpper currChar : [currChar]
        thisCount = runner $ filter (not . flip elem tester) hunk
    in 
       if thisCount < winningCount then thisCount else winningCount 

doer :: String -> String
doer = foldl folder [] 

runner :: String -> Int
runner x =
    length $ runHelp x $ length x 
    
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
 
