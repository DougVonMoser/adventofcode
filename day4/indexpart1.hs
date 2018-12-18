import Data.List.Split
import Data.Maybe
import Text.Read
import Debug.Trace
import Data.List
import qualified Data.Map.Strict as Map

main = do 
    input <- readFile "input.txt"
    print
            $ finishHim
            $ (\(_,_,x) -> x)
            $ foldl foldingFunc (Down, 0, Map.empty)
            $ sortBy (\(t1, _) (t2, _) -> compare t1 t2) 
            $ map parseInput
            $ lines input

finishHim :: Stuf -> Int
finishHim hunk = 
    let winningBadge = fst $  Map.foldlWithKey (\(bestbadge, x) currbadge m -> 
                                            let total = Map.foldl (+) 0 m  
                                            in if total > x 
                                               then (currbadge, total) 
                                               else (bestbadge, x) )
                              (69, 0) hunk
        winningMap = Map.lookup winningBadge hunk
    in 
        case winningMap of 
            Just wm -> (*) winningBadge $ fst $ Map.foldlWithKey (\(winningminute, winTotal) minute total -> 
                                                                    if total > winTotal
                                                                     then (minute, total) 
                                                                     else (winningminute, winTotal) ) (0,0) wm 
            Nothing -> 69

type Stuf = Map.Map Int ( Map.Map Int Int)
type Id = Int 
type Date = (Int, Int, Int)
type Time = Int   -- representing minute past midnight
data GuardLog = BeginShift Id | Down | Up deriving (Show, Eq)

foldingFunc :: (GuardLog, Int, Stuf) -> ([Int], GuardLog) -> (GuardLog, Int, Stuf)
foldingFunc (_ , _, bigmap)                   ([_,_,_,_,_],BeginShift badge) = (BeginShift badge, 0, bigmap)
foldingFunc (shift , _, bigmap)               ([_,_,_,_,downminute], Down)   = (shift, downminute, bigmap)
foldingFunc (BeginShift badge, downtime, bigmap) ([_,_,_,_,upminute], Up)       = 
    let currentmap = foldl (\map x -> Map.insert x 1 map) Map.empty [downtime..upminute - 1]
        newbigmap = 
            if Map.member badge bigmap 
            then Map.adjust (Map.unionWith (+) currentmap) badge bigmap
            else Map.insert badge currentmap bigmap
    in (BeginShift badge, 0, newbigmap)

-- parseInput :: String -> (Date, Time, GuardLog)
parseInput s = 
    keepSplitting $ endByOneOf "]" s

-- keepSplitting :: [String] -> (Date, Time, GuardLog)
keepSplitting [datetime, action] =
    (workDateTime datetime, workaction action)

workDateTime s = 
    catMaybes $ (map (\x -> readMaybe x :: Maybe Int)) $ endByOneOf "[- :" s

workaction " falls asleep" = Down
workaction " wakes up" = Up
workaction x = 
    BeginShift $ (\x -> read x :: Int) $ reverse $ drop 13 $ reverse $ drop 8 x


