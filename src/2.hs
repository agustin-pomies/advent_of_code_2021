import Prelude hiding (product) 

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

solveWithXY :: [Movement] -> IO Int
solveWithXY movements = return $ let final_x = sum $ map fst movements
                                     final_y = sum $ map snd movements
                                 in final_x * final_y

type Aim = Int
type Position = (Int, Int)
type Submarine = (Aim, Position)

modifySubmarine :: Submarine -> Movement -> Submarine
modifySubmarine (aim, position) (0, units)    = (aim + units, position)
modifySubmarine (aim, (x, y))   (units, 0)    = (aim, (x + units, y + aim * units))

product :: Position -> Int
product pair = fst pair * snd pair

solveWithAim :: [Movement] -> IO Int
solveWithAim movements = return $ product . snd $ foldl modifySubmarine (0,(0,0)) movements

data Strategy = XY | Aim

solveWith :: Strategy -> [Movement] -> IO Int
solveWith XY  = solveWithXY
solveWith Aim = solveWithAim

-- Main Program
main :: IO ()
main = do
  putStrLn $ "Solving test data..."
  filedata <- readInput testPath
  problemData <- processInput filedata
  answer <- solveWith XY problemData
  putStrLn $ "Test answer using XY is " ++ (show answer)
  putStrLn $ "Solving Part 1..."
  filedata <- readInput inputPath
  problemData <- processInput filedata
  answer <- solveWith XY problemData
  putStrLn $ "Part 1 answer is " ++ (show answer)
  putStrLn $ "* * * * * * * * *"
  putStrLn $ "Solving test data..."
  filedata <- readInput testPath
  problemData <- processInput filedata
  answer <- solveWith Aim problemData
  putStrLn $ "Test answer using Aim is " ++ (show answer)
  putStrLn $ "Solving Part 2..."
  filedata <- readInput inputPath
  problemData <- processInput filedata
  answer <- solveWith Aim problemData
  putStrLn $ "Part 2 answer is " ++ (show answer)
