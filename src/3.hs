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

-- Datatypes & Data parsing
intToBool :: Char -> Bool
intToBool = (/= 0) . read . pure

type Binaries = [Binary]
type Binary = [Bool]

processInput :: [String] -> IO Binaries
processInput filedata = return $ map (\s -> map intToBool s) filedata

-- Logic Handling
mostCommon :: Ord a => [a] -> a
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

leastCommon :: Ord a => [a] -> a
leastCommon = snd . minimum . map (\xs -> (length xs, head xs)) . group . sort

binToDec :: Binary -> Int
binToDec = foldl (\x y -> fromEnum y + 2*x) 0

powerConsumption :: Binaries -> IO Int
powerConsumption binaries = return $ let columns = transpose binaries
                                         gammaRate = binToDec (map mostCommon columns)
                                         epsilonRate = binToDec (map leastCommon columns)
                                     in gammaRate * epsilonRate

reduceOptions :: Binaries -> (Binary -> Bool) -> Int -> Binary
reduceOptions [binary] _ _    = binary
reduceOptions binaries f pos  = let column = map (!! pos) binaries
                                    examinedDigit = f column
                                in reduceOptions (filter ((== examinedDigit) . (!!pos)) binaries) f (pos + 1)

lifeSupportRating :: Binaries -> IO Int
lifeSupportRating binaries = return $ let oxygenGeneratorRating = binToDec $ reduceOptions binaries mostCommon 0
                                          co2ScrubberRating = binToDec $ reduceOptions binaries leastCommon 0
                                      in co2ScrubberRating * oxygenGeneratorRating

-- Main Program
main :: IO ()
main = do
  putStrLn $ "Solving test data..."
  filedata <- readInput testPath
  report <- processInput filedata
  answer <- powerConsumption report
  putStrLn $ "Power Consumption from example is " ++ (show answer)
  putStrLn $ "Solving Part 1..."
  filedata <- readInput inputPath
  report <- processInput filedata
  answer <- powerConsumption report
  putStrLn $ "Power Consumption is " ++ (show answer)
  putStrLn $ "* * * * * * * * *"
  putStrLn $ "Solving test data..."
  filedata <- readInput testPath
  report <- processInput filedata
  answer <- lifeSupportRating report
  putStrLn $ "Life Support Rating from example is " ++ (show answer)
  putStrLn $ "Solving Part 2..."
  filedata <- readInput inputPath
  report <- processInput filedata
  answer <- lifeSupportRating report
  putStrLn $ "Life Support Rating is " ++ (show answer)
