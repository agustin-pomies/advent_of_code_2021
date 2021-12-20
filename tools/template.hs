-- Input & Output
basePath :: String
basePath = "/mnt/c/Users/dlabs/Documents/Personal/advent_of_code_2021/"

testPath :: String
testPath = basePath ++ "examples/DAY_NUMBER.txt"

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
main :: IO ()
main = do
  putStrLn $ "Solving test data..."
  filedata <- readInput testPath
  problemData <- processInput filedata
  answer <- solve problemData
  putStrLn $ "Test answer is " ++ (show answer)
  putStrLn $ "Solving Part 1..."
  filedata <- readInput inputPath
  problemData <- processInput filedata
  answer <- solve problemData
  putStrLn $ "Part 1 answer is " ++ (show answer)
  putStrLn $ "Solving Part 2..."
  filedata <- readInput inputPath
  problemData <- processInput filedata
  answer <- solve problemData
  putStrLn $ "Part 2 answer is " ++ (show answer)
