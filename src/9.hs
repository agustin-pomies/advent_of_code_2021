module Day9 where
  import Data.Char (digitToInt)
  import Data.List (delete)

  -- Input & Output
  basePath :: String
  basePath = "/mnt/c/Users/dlabs/Documents/Personal/advent_of_code_2021/"

  inputPath :: String
  inputPath = basePath ++ "inputs/9.txt"

  readInput :: String -> IO [String]
  readInput filePath = do
    content <- readFile filePath
    return $ lines content

  -- Datatypes & Data parsing
  type HeightMap = [[Int]]
  type Dimensions = (Int, Int)

  data Model = Grid HeightMap Dimensions deriving (Show)

  heightmap :: Model -> HeightMap
  heightmap (Grid heightmap _) = heightmap 

  dimensions :: Model -> Dimensions
  dimensions (Grid _ dims) = dims 

  processInput :: [[Char]] -> IO Model
  processInput filedata = return $ Grid (map (map digitToInt) filedata) (length filedata, (length . head) filedata)

  -- Logic Handling
  type Position = (Int, Int)

  crossNeighbours :: Position -> [Position]
  crossNeighbours (x, y) = [(x+a, y+b) | a <- [-1..1], b <- [-1..1], a * b == 0, a /= b]

  inBoard :: Dimensions -> Position -> Bool
  inBoard (rows, columns) (x, y) = (x >= 0 && x <= rows - 1) && (y >= 0 && y <= columns - 1) 

  crlf :: Position -> Position
  crlf (x, y) = (x+1, 0)

  next :: Position -> Position
  next (x, y) = (x, y+1)

  heightInAt :: HeightMap -> Position -> Int
  heightInAt heightmap (x, y) = (heightmap !! x) !! y

  evalNeighbours :: [Int] -> [Position] -> Position -> Model -> [Int]
  evalNeighbours lowPoints toBeRejected pos model =
    let heightPos = heightInAt (heightmap model) pos
        gridNeighbours = filter (inBoard (dimensions model)) (crossNeighbours pos)
        rejectedNeighbours = filter ((> heightPos) . (heightInAt (heightmap model))) gridNeighbours
        addedLowPoint = if gridNeighbours == rejectedNeighbours then [heightPos] else []
    in accLowPoints (lowPoints ++ addedLowPoint) (toBeRejected ++ rejectedNeighbours) (next pos) model

  evalCandidate :: [Int] -> [Position] -> Position -> Model -> [Int]
  evalCandidate lowPoints toBeRejected pos model =
    if elem pos toBeRejected
    then accLowPoints lowPoints (delete pos toBeRejected) (next pos) model
    else evalNeighbours lowPoints toBeRejected pos model

  accLowPoints :: [Int] -> [Position] -> Position -> Model -> [Int]
  accLowPoints lowPoints toBeRejected pos@(x, y) model
    | fst (dimensions model) == fst pos   = lowPoints
    | snd (dimensions model) == y         = accLowPoints lowPoints toBeRejected (crlf pos) model
    | otherwise                           = evalCandidate lowPoints toBeRejected pos model

  findLowPoints :: Model -> [Int]
  findLowPoints = accLowPoints [] [] (0,0)

  riskLevel :: Int -> Int
  riskLevel = (+1)

  solve :: Model -> IO Int
  solve model = let lowPoints = findLowPoints model
                in return $ sum (map riskLevel lowPoints)

  -- Main Program
  part1 :: IO ()
  part1 = do
    putStrLn $ "Solving Part 1..."
    filedata <- readInput inputPath
    problemData <- processInput filedata
    answer <- solve problemData
    putStrLn $ "The sum of the risk levels of all low points on the heightmap is " ++ (show answer)

  part2 :: IO ()
  part2 = undefined

  main :: IO ()
  main = do
    part1
