module Day8 where
  import Data.List.Split (splitOn)

  -- Input & Output
  basePath :: String
  basePath = "/mnt/c/Users/dlabs/Documents/Personal/advent_of_code_2021/"

  inputPath :: String
  inputPath = basePath ++ "inputs/8.txt"

  readInput :: String -> IO [String]
  readInput filePath = do
    content <- readFile filePath
    return $ lines content

  -- Datatypes & Data parsing
  type Signal = String
  type Patterns = [Signal]
  type Entry = (Patterns, Patterns)
  type Notes = [Entry]

  tuplify2 :: [a] -> (a,a)
  tuplify2 [x,y] = (x,y)

  processLine :: String -> Entry
  processLine line = (tuplify2 . (map words) . (splitOn "|")) line

  processInput :: [String] -> IO Notes
  processInput filedata = return $ map processLine filedata

  -- Logic Handling
  easyDigitDisplay :: [Int]
  easyDigitDisplay = [2, 3, 4, 7]

  isEasyDigit :: Signal -> Bool
  isEasyDigit pattern = elem (length pattern) easyDigitDisplay

  solve :: Notes -> IO Int
  solve pattern = return $ (length . (filter isEasyDigit) . concat . (map snd)) pattern

  -- Main Program
  part1 :: IO ()
  part1 = do
    putStrLn $ "Solving Part 1..."
    filedata <- readInput inputPath
    problemData <- processInput filedata
    answer <- solve problemData
    putStrLn $ "Numbers 1, 4, 7 and 8 appear " ++ (show answer) ++ " times"

  part2 :: IO ()
  part2 = undefined

  main :: IO ()
  main = do
    part1
