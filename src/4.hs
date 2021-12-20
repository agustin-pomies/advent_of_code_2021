import Data.List((\\), group, groupBy, inits, transpose, uncons)
import Data.List.Split(splitOn)
import Data.Maybe

-- Input & Output
basePath :: String
basePath = "/mnt/c/Users/dlabs/Documents/Personal/advent_of_code_2021/"

testPath :: String
testPath = basePath ++ "examples/4.txt"

inputPath :: String
inputPath = basePath ++ "inputs/4.txt"

readInput :: String -> IO [String]
readInput filePath = do
  content <- readFile filePath
  return $ lines content

-- Datatypes & Data parsing
type Numbers = [Int]
type Board = [[Int]]
type Bingo = (Numbers, [Board])
type Combination = [Int]

processNumbers :: String -> Numbers
processNumbers = (map read) . (splitOn ",")

processBoard :: [String] -> Board
processBoard rows = map (map read . words) rows

processBoards :: [[String]] -> [Board]
processBoards = map processBoard

combine :: String -> [[String]] -> String -> [[String]]
combine sep [] elem = if elem == sep then [[]] else [[elem]]
combine sep groups@(x:xs) elem = if elem == sep
                                 then [] : groups
                                 else ((elem : x) : xs)

separateBy :: String -> [String] -> [[String]]
separateBy sep = foldl (combine sep) []

separateInput :: [String] -> ([String], [[String]])
separateInput = fromJust . uncons . reverse . separateBy ""

processElements :: ([String], [[String]]) -> Bingo
processElements (x, y) = ((processNumbers . head) x, processBoards y) 

processInput :: [String] -> IO Bingo
processInput lines = return $ (processElements . separateInput) lines

-- Logic Handling
combinations :: Board -> [Combination]
combinations board = board ++ transpose board

winnerCombination :: Numbers -> Board -> Combination
winnerCombination numbers board = let boardCombinations = combinations board
                                      hasNumbers = (== 0) . length . (flip (\\)) numbers
                                      filtered = filter hasNumbers boardCombinations
                                  in if null filtered then [] else head filtered

isWinner :: Numbers -> Board -> Bool
isWinner numbers = ((/= 0) . length . winnerCombination numbers)

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p [] = []
takeWhileOneMore p (x:xs) = 
   if p x
   then x : takeWhileOneMore p xs
   else [x]

determineWinnerSequence :: Bingo -> Numbers
determineWinnerSequence (numbers, boards) = last $ takeWhileOneMore f (inits numbers)
  where f = \init -> (null . concat) (map (winnerCombination init) boards)

determineWinnerBoard :: Numbers -> [Board] -> Board 
determineWinnerBoard sequence boards = head $ filter (isWinner sequence) boards

determineWinnerSolution :: Bingo -> (Numbers, Board) 
determineWinnerSolution bingo@(numbers, boards) = let winnerSequence = determineWinnerSequence bingo
                                                      winnerBoard = determineWinnerBoard winnerSequence boards
                                                  in (winnerSequence, winnerBoard)

determineLoserSequence :: Bingo -> Numbers
determineLoserSequence (numbers, boards) = last $ takeWhile f (reverse $ inits numbers)
  where f = \init -> all ((/= 0) . length) (map (winnerCombination init) boards)

determineLoserBoard :: Numbers -> [Board] -> Board 
determineLoserBoard sequence boards = head $ filter (not . isWinner (init sequence)) boards

determineLoserSolution :: Bingo -> (Numbers, Board) 
determineLoserSolution bingo@(numbers, boards) = let loserSequence = determineLoserSequence bingo
                                                     loserBoard = determineLoserBoard loserSequence boards
                                                 in (loserSequence, loserBoard)

score :: (Numbers, Board) -> IO Int
score (numbers, board) = let unmarkedNumbers = filter ((flip notElem) numbers) (concat board)
                             lastCalled = last numbers
                         in return $ sum unmarkedNumbers * lastCalled

-- Main Program
main :: IO ()
main = do
  putStrLn $ "Solving test data..."
  filedata <- readInput testPath
  problemData <- processInput filedata
  answer <- score $ determineWinnerSolution problemData 
  putStrLn $ "Final score for winner board from example is " ++ (show answer)
  putStrLn $ "Solving Part 1..."
  filedata <- readInput inputPath
  problemData <- processInput filedata
  answer <- score $ determineWinnerSolution problemData
  putStrLn $ "Final score for winner board is " ++ (show answer)
  putStrLn $ "Solving test data..."
  filedata <- readInput testPath
  problemData <- processInput filedata
  answer <- score $ determineLoserSolution problemData 
  putStrLn $ "Final score for loser board from example is " ++ (show answer)
  putStrLn $ "Solving Part 2..."
  filedata <- readInput inputPath
  problemData <- processInput filedata
  answer <- score $ determineLoserSolution problemData
  putStrLn $ "Final score for loser board is " ++ (show answer)

