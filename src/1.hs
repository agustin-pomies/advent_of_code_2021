-- Input & Output
basePath :: String
basePath = "/mnt/c/Users/dlabs/Documents/Personal/advent_of_code_2021/"

testPath :: String
testPath = basePath ++ "tests/1.txt"

inputPath :: String
inputPath = basePath ++ "inputs/1.txt"

readInput :: String -> IO [String]
readInput filePath = do
  content <- readFile filePath
  return $ lines content

processInput :: [String] -> IO [Int]
processInput filedata = return $ map read filedata

compactAndCompare :: Num a => [a] -> [a]
compactAndCompare [] = []
compactAndCompare [x] = []
compactAndCompare (x : y : xs) = (y - x) : compactAndCompare (y : xs)

-- Solution
solve :: [Int] -> IO Int
solve depths = do
  return $ length $ filter (> 0) (compactAndCompare depths)

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
