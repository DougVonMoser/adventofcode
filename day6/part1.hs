import           Data.List
import           Data.List.Split
import qualified Data.Map.Strict as Map

type Point = (Int, Int)

data Cell = Empty Point | Place Char Point | CalculatedClosest Char Point Int | EquaDistant Int

instance Show Cell where
    show (CalculatedClosest c p i) = show c
    show (EquaDistant _d)          = show '.'

main = do
    input <- readFile "input.txt"
    print $ main'
          $ map ((\[x,y] -> (x,y)) . interize . (splitOn ", "))
          $ lines input

main' x = someFlow x



findPerimeter :: Point -> Point -> [Cell] -> [Char]
findPerimeter (left,top) (right,bottom) calkedGrid =
   map (\(CalculatedClosest c _ _) -> c) $ filter (perimeterFilter left top right bottom) calkedGrid


perimeterFilter left top right bottom (CalculatedClosest c (x, y) d)
    | x == left        = True
    | x == right       = True
    | y == top         = True
    | y == bottom      = True
    | otherwise        = False

perimeterFilter _ _ _ _ (EquaDistant _) = False

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1,x2) (y1, y2) =
    abs (x1 - y1) + abs (x2 - y2)

someFlow points =
    let ( (minX, minY), (maxX, maxY) ) = findBoundary points
        grid = concat $ map (buildTiny minX maxX) [minY .. maxY]
        labeledPoints = snd $ mapAccumL (\acc x -> (succ acc, Place acc x )) 'A' points
        calkedGrid = map (\point -> foldl (manhattanFold point) initClosest labeledPoints) grid
        badChars = findPerimeter (minX, minY) (maxX, maxY) calkedGrid
        charMapper = foldl foldingCounter Map.empty calkedGrid
        remainingVals = Map.filterWithKey (\k v -> k `notElem` badChars) charMapper
    in Map.foldl max (minBound :: Int) remainingVals

foldingCounter map (CalculatedClosest c _ _) =
    if Map.member c map then
        Map.adjust ((+) 1) c map
    else
        Map.insert c 1 map

foldingCounter map _ = map

manhattanFold point (CalculatedClosest c p d) (Place lc lp) =
   let howlongboi = manhattanDistance point lp
   in case compare howlongboi d of
        LT -> CalculatedClosest lc point howlongboi
        GT -> CalculatedClosest c p d
        EQ -> EquaDistant howlongboi

manhattanFold point (EquaDistant d) (Place lc lp) =
   let howlongboi = manhattanDistance point lp

    in  if howlongboi < d then
                     CalculatedClosest lc point howlongboi
    else
        EquaDistant d

initClosest = (CalculatedClosest 'V' (0,0)  (maxBound :: Int))

buildTiny :: Int -> Int ->  Int -> [(Int,Int)]
buildTiny  minX maxX y =
    map (\x -> (x,y)) [minX .. maxX]


findBoundary :: [(Int, Int)] -> (Point, Point)
findBoundary points =
    let
        xMin = foldl (\acc (x,y) -> min acc x) (maxBound :: Int) points
        yMin = foldl (\acc (x,y) -> min acc y) (maxBound :: Int) points
        xMax = foldl (\acc (x,y) -> max acc x) (minBound :: Int) points
        yMax = foldl (\acc (x,y) -> max acc y) (minBound :: Int) points
    in
        ( (xMin, yMin), (xMax, yMax) )

interize :: [String] -> [Int]
interize = map (\x -> read x :: Int)

