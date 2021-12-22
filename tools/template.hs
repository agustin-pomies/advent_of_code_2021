module DayDAY_NUMBER where
  -- Input & Output
  basePath :: String
  basePath = "/mnt/c/Users/dlabs/Documents/Personal/advent_of_code_2021/"

  inputPath :: String
  inputPath = basePath ++ "inputs/DAY_NUMBER.txt"

  readInput :: String -> IO [String]
  readInput filePath = do
    content <- readFile filePath
    return $ lines content

  -- Datatypes & Data parsing
  processInput :: [String] -> IO [Int]
  processInput filedata = return $ map read filedata

  -- Logic Handling
  solve :: () -> IO ()
  solve = undefined

  -- Main Program
  part1 :: IO ()
  part1 = do
    putStrLn $ "Solving Part 1..."
    filedata <- readInput inputPath
    problemData <- processInput filedata
    answer <- solve problemData
    putStrLn $ "Part 1 answer is " ++ (show answer)

  part2 :: IO ()
  part2 = undefined

  main :: IO ()
  main = do
    part1
