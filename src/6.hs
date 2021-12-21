module Lanternfish where
  import Data.List
  import Data.List.Split
  import Data.Map (Map, fromList, mapKeysWith)
  import qualified Data.Map as Map
  import Data.Maybe

  -- Input & Output
  basePath :: String
  basePath = "/mnt/c/Users/dlabs/Documents/Personal/advent_of_code_2021/"

  inputPath :: String
  inputPath = basePath ++ "inputs/6.txt"

  readInput :: String -> IO [String]
  readInput filePath = do
    content <- readFile filePath
    return $ lines content

  -- Datatypes & Data parsing
  type LanternfishGroup = (Int, Int)
  type Population = Map Int Int -- Map Value Count

  processInput :: [String] -> IO Population
  processInput filedata = return $ (fromList . frequencies . (map read) . (splitOn ",") . head) filedata

  -- Logic Handling
  frequencies :: Ord a => [a] -> [(a, Int)]
  frequencies = map (\xs -> (head xs, length xs)) . group . sort

  updateTimers :: Int -> Int
  updateTimers num = ((if num <= 6 then (flip mod) 7 else id) . (subtract 1)) num

  nextGen :: Population -> Population
  nextGen population = let newLanternfishes = fromMaybe 0 (Map.lookup 0 population)
                           newPopulation = mapKeysWith (+) updateTimers population
                           newGeneration = Map.insert 8 newLanternfishes newPopulation
                       in newGeneration

  simulate :: Population -> Int -> Population
  simulate population 0    = population
  simulate population days = simulate (nextGen population) (days - 1)

  solve :: Population -> Int -> IO Int
  solve population days = return $ Map.fold (+) 0 (simulate population days)

  -- Main Program
  part1 :: IO ()
  part1 = do
    putStrLn $ "Solving Part 1..."
    filedata <- readInput inputPath
    problemData <- processInput filedata
    answer <- solve problemData 80
    putStrLn $ "Lanternfish population on day 80 is " ++ (show answer)

  part2 :: IO ()
  part2 = do
    putStrLn $ "Solving Part 2..."
    filedata <- readInput inputPath
    problemData <- processInput filedata
    answer <- solve problemData 256
    putStrLn $ "Lanternfish population on day 256 is " ++ (show answer)

  main :: IO ()
  main = do
    part1
    part2
