import Data.List (group, sort, sortBy)
import Data.List.Split (splitOn)

-- Input & Output
basePath :: String
basePath = "/mnt/c/Users/dlabs/Documents/Personal/advent_of_code_2021/"

testPath :: String
testPath = basePath ++ "tests/5.txt"

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

-- Precondition: Segment has edges in order
covers :: Segment -> [Point]
covers ((a, b), (c, d)) = [(i, j) | i <- [a..c], j <- [b..d]]

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
