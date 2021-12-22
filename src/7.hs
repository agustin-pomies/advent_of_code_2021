module Day7 where
  import Data.Either
  import Data.List (sort, minimumBy)
  import Data.List.Split (splitOn)
  import Data.Ord (comparing)

  -- Input & Output
  basePath :: String
  basePath = "/mnt/c/Users/dlabs/Documents/Personal/advent_of_code_2021/"

  testPath :: String
  testPath = basePath ++ "examples/7.txt"

  inputPath :: String
  inputPath = basePath ++ "inputs/7.txt"

  readInput :: String -> IO [String]
  readInput filePath = do
    content <- readFile filePath
    return $ lines content

  -- Datatypes & Data parsing
  type Position = Int

  processInput :: [String] -> IO [Position]
  processInput filedata = return $ ((map read) . (splitOn ",") . head) filedata

  -- Logic Handling
  min' :: Ord b => (a -> b) -> [a] -> (a, b)
  min' f candidates = let values = map f candidates
                          possibilities = zip candidates values
                      in minimumBy (comparing snd) possibilities

  alignCost :: (Int -> Int -> Int) -> [Int] -> Int -> Int
  alignCost distance set candidate = sum $ map (distance candidate) set

  distance1D :: Int -> Int -> Int
  distance1D a b = abs (a - b)

  distanceJumps :: Int -> Int -> Int
  distanceJumps a b = let distance = distance1D a b
                      in distance * (distance + 1) `div` 2

  optimalAlignment :: (Position -> Position -> Int) -> [Position] -> (Position, Int)
  optimalAlignment distance set = min' (alignCost distance set) [0..maximum set]

  solve :: (Position -> Position -> Int) -> [Position] -> IO Position
  solve distance set = return $ snd (optimalAlignment distance set)

  -- Main Program
  part1 :: IO ()
  part1 = do
    putStrLn $ "Solving Part 1..."
    filedata <- readInput inputPath
    problemData <- processInput filedata
    answer <- solve distance1D problemData
    putStrLn $ "Total fuel consumed on cheapest alignment with normal distance is " ++ (show answer)

  part2 :: IO ()
  part2 = do
    putStrLn $ "Solving Part 2..."
    filedata <- readInput inputPath
    problemData <- processInput filedata
    answer <- solve distanceJumps problemData
    putStrLn $ "Total fuel consumed on cheapest alignment with distance jumps is " ++ (show answer)

  main :: IO ()
  main = do
    part1
    part2
