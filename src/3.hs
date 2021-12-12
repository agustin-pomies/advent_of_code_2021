import Data.List

-- Input & Output
basePath :: String
basePath = "/mnt/c/Users/dlabs/Documents/Personal/advent_of_code_2021/"

testPath :: String
testPath = basePath ++ "tests/3.txt"

inputPath :: String
inputPath = basePath ++ "inputs/3.txt"

readInput :: String -> IO [String]
readInput filePath = do
  content <- readFile filePath
  return $ lines content

intToBool :: Char -> Bool
intToBool = (/= 0) . read . pure

type Binaries = [Binary]
type Binary = [Bool]

processInput :: [String] -> IO Binaries
processInput filedata = return $ map (\s -> map intToBool s) filedata

mostCommon :: Ord a => [a] -> a
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

leastCommon :: Ord a => [a] -> a
leastCommon = snd . minimum . map (\xs -> (length xs, head xs)) . group . sort

binToDec :: Binary -> Int
binToDec = foldl (\x y -> fromEnum y + 2*x) 0

solve :: Binaries -> IO Int
solve binaries = return $ let columns = transpose binaries
                              gammaRate = binToDec (map mostCommon columns)
                              epsilonRate = binToDec (map leastCommon columns)
                              powerConsumption = gammaRate * epsilonRate
                          in powerConsumption

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
