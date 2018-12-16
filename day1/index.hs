import System.IO
import Data.Maybe
import Text.Read
import qualified Data.Set as Set
import qualified Data.List as List

main = do
    input <- readFile "input.txt"
    print $ head
          $ dropWhile isNothing 
          $ snd
          $ List.mapAccumL foldingFunc (Set.empty, 0)  
          $ cycle
          $ catMaybes 
          $ map ( readMaybe . map replacePlus )
          $ lines input

type SetAccum = (Set.Set Int, Int)
 
foldingFunc :: SetAccum  -> Int -> (SetAccum, Maybe Int)
foldingFunc (s, total) curr  = 
    let newTotal = total + curr 
    in 
        if Set.member newTotal s 
        then ((s, newTotal), Just newTotal)
        else ((Set.insert newTotal s, newTotal), Nothing) 

replacePlus :: Char -> Char
replacePlus '+' = ' '
replacePlus  c = c

