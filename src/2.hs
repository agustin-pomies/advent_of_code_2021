import Data.List

-- Input & Output
basePath :: String
basePath = "/mnt/c/Users/dlabs/Documents/Personal/advent_of_code_2021/"

testPath :: String
testPath = basePath ++ "tests/2.txt"

inputPath :: String
inputPath = basePath ++ "inputs/2.txt"

readInput :: String -> IO [String]
readInput filePath = do
  content <- readFile filePath
  return $ lines content

type Movement = (Int, Int)

processCommand :: String -> Movement
processCommand command =
  case words command of
    ("forward"  : units : []) -> (read units, 0)
    ("up"       : units : []) -> (0, - read units) 
    ("down"     : units : []) -> (0, read units)

processInput :: [String] -> IO [Movement]
processInput filedata = return $ map processCommand filedata

solve :: [Movement] -> IO Int
solve movements = return $ let final_x = sum $ map fst movements
                               final_y = sum $ map snd movements
                           in final_x * final_y

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
