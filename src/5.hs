import Data.List (group, sort, sortBy)
import Data.List.Split (splitOn)

-- Input & Output
basePath :: String
basePath = "/mnt/c/Users/dlabs/Documents/Personal/advent_of_code_2021/"

testPath :: String
testPath = basePath ++ "examples/5.txt"

inputPath :: String
inputPath = basePath ++ "inputs/5.txt"

readInput :: String -> IO [String]
readInput filePath = do
  content <- readFile filePath
  return $ lines content

-- Datatypes & Data parsing
type Point = (Int, Int)
type Segment = (Point, Point)

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

processEdge :: String -> Point
processEdge edge = tuplify2 $ map read (splitOn "," edge)

processCoord :: (String, String) -> Segment
processCoord (start, end) = (tuplify2 . sortBy compare) [processEdge start, processEdge end]

processInput :: [String] -> IO [Segment]
processInput lines = return $ map (processCoord . tuplify2 . splitOn " -> ") lines

-- Logic Handling
isStraight :: Segment -> Bool
isStraight ((a, b), (c, d))
  | a == c    = True  -- vertical
  | b == d    = True  -- horizontal
  | otherwise = False -- diagonal

slope :: Segment -> Int
slope ((a, b), (c, d))
  | b == d      = 0
  | a == c      = maxBound
  | otherwise   = (d - b) `div` (c - a)

-- Precondition: Segment has edges in order
covers :: Segment -> [Point]
covers segment@((a, b), (c, d)) =
  case slope segment of
    -1        -> zip [a..c] (enumFromThenTo b (b-1) d)
    0         -> [(i, b) | i <- [a..c]]
    1         -> zip [a..c] [b..d]
    maxBound  -> [(a, j) | j <- [b..d]]

frequencies :: Ord a => [a] -> [(Int, a)]
frequencies = map (\xs -> (length xs, head xs)) . group . sort

overlappingPoints :: [Segment] -> IO Int
overlappingPoints segments = return $ (length . (filter ((>1) . fst)) . frequencies . concat . map covers) segments

-- Main Program
main :: IO ()
main = do
  putStrLn $ "Solving test data..."
  filedata <- readInput testPath
  problemData <- processInput filedata
  answer <- overlappingPoints (filter isStraight problemData)
  putStrLn $ "Number of points where straight lines overlap from example are " ++ (show answer)
  putStrLn $ "Solving Part 1..."
  filedata <- readInput inputPath
  problemData <- processInput filedata
  answer <- overlappingPoints (filter isStraight problemData)
  putStrLn $ "Number of points where straight lines overlap are " ++ (show answer)
  putStrLn $ "Solving test data..."
  filedata <- readInput testPath
  problemData <- processInput filedata
  answer <- overlappingPoints problemData
  putStrLn $ "Number of points where lines overlap from example are " ++ (show answer)
  putStrLn $ "Solving Part 2..."
  filedata <- readInput inputPath
  problemData <- processInput filedata
  answer <- overlappingPoints problemData
  putStrLn $ "Number of points where lines overlap are " ++ (show answer)
