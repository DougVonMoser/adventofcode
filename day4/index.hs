import Data.List.Split
import Data.Maybe
import Text.Read
import Debug.Trace

main = do 
    input <- readFile "test.txt"
    print   $ map parseInput
            $ lines input

type Id = Int 
type Date = (Int, Int, Int)
type Time = Int   -- representing minute past midnight
data GuardLog = BeginShift Id | Down | Up deriving (Show, Eq)

-- parseInput :: String -> (Date, Time, GuardLog)
parseInput s = 
    keepSplitting $ endByOneOf "]" s

-- keepSplitting :: [String] -> (Date, Time, GuardLog)
keepSplitting [datetime, action] =
    (workDateTime datetime, workaction action)

workDateTime s = 
    catMaybes $ (map (\x -> readMaybe x :: Maybe Int)) $ endByOneOf "[- :" s

workaction s = 
    Down


