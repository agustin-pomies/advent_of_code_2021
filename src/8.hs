module Day8 where
  import Data.List((\\), delete, groupBy, intersect, sort)
  import Data.List.Split (splitOn)
  import Data.Map (Map, (!))
  import qualified Data.Map as Map

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
  digitPatterns :: Map Signal Int
  digitPatterns = Map.fromList [
      ("cf", 1),      -- 2
      ("acf", 7),     -- 3
      ("bcdf", 4),    -- 4
      ("acdeg", 2),   -- 5
      ("acdfg", 3),
      ("abdfg", 5),
      ("abcefg", 0),  -- 6
      ("abdefg", 6),
      ("abcdfg", 9),
      ("abcdefg", 8)  -- 7
    ]

  numSegmentsUsed :: Map Int [Int]
  numSegmentsUsed = Map.mapKeysWith (++) length (Map.map ((flip (:)) []) digitPatterns)

  isEasyDigit :: Signal -> Bool
  isEasyDigit = ((== 1) . length . ((!) numSegmentsUsed) . length)

  countEasyDigits :: Notes -> IO Int
  countEasyDigits notes = return $ (length . (filter isEasyDigit) . concat . (map snd)) notes

  selectPattern :: Int -> Patterns -> Signal
  selectPattern num = head . (filter ((== num) . length))

  isSublistOf :: (Eq a) => [a] -> [a] -> Bool
  l1 `isSublistOf` l2 = intersect l1 l2 == l1

  type WireCrossing = Map Char Char

  educatedDeduction :: Patterns -> WireCrossing
  educatedDeduction patterns =
    let noisySeven              = selectPattern 3 patterns
        noisyOne                = selectPattern 2 patterns
        noisyTopSegment         = head (noisySeven \\ noisyOne)
        noisyFour               = selectPattern 4 patterns
        noisySixSegments        = filter ((== 6) . length) patterns
        noisyNine               = (head . (filter (noisyFour `isSublistOf`))) noisySixSegments
        noisyFiveSegments       = filter ((== 5) . length) patterns
        noisyCenterSegment      = head (foldl1 intersect (noisyFour : noisyFiveSegments))
        noisyBottomLeftSegment  = head (foldl1 intersect (delete noisyNine noisySixSegments) \\ noisyNine)
        noisyBottomSegment      = head ((foldl1 intersect noisyFiveSegments) \\ [noisyCenterSegment, noisyTopSegment])
        incompleteTwo           = [noisyTopSegment, noisyCenterSegment, noisyBottomLeftSegment, noisyBottomSegment]
        noisyTopRightSegment    = head (head (filter ((== 1) . length) (map ((flip (\\)) incompleteTwo) noisyFiveSegments)))
        noisyTwo                = noisyTopRightSegment : incompleteTwo
        noisyThree              = head (filter ((== 1) . length . ((flip (\\)) noisyTwo)) noisyFiveSegments)
        noisyBottomRightSegment = head (noisyThree \\ noisyTwo)
        noisyTopLeftSegment     = head (noisyNine \\ noisyThree)
    in Map.fromList [
      (noisyTopSegment,         'a'),
      (noisyTopLeftSegment,     'b'),
      (noisyTopRightSegment,    'c'),
      (noisyCenterSegment,      'd'),
      (noisyBottomLeftSegment,  'e'),
      (noisyBottomRightSegment, 'f'),
      (noisyBottomSegment,      'g')
    ]

  deduceWireCrossing :: Patterns -> WireCrossing
  deduceWireCrossing = educatedDeduction

  signalToDigit :: WireCrossing -> Signal -> Int
  signalToDigit mapping noisySignal = let signal = map ((!) mapping) noisySignal
                                      in (((!) digitPatterns) . sort) signal

  undigits :: Integral n => [n] -> n
  undigits = foldl (\a b -> a * 10 + b) 0

  patternsToOutput :: WireCrossing -> Patterns -> Int
  patternsToOutput mapping patterns = undigits $ map (signalToDigit mapping) patterns

  entryOutput :: Entry -> Int
  entryOutput (sample, noisyOutput) = let mapping = deduceWireCrossing sample
                                      in patternsToOutput mapping noisyOutput

  solve :: Notes -> IO Int
  solve notes = return $ sum (map entryOutput notes)

  -- Main Program
  part1 :: IO ()
  part1 = do
    putStrLn $ "Solving Part 1..."
    filedata <- readInput inputPath
    problemData <- processInput filedata
    answer <- countEasyDigits problemData
    putStrLn $ "Numbers 1, 4, 7 and 8 appear " ++ (show answer) ++ " times"

  part2 :: IO ()
  part2 = do
    putStrLn $ "Solving Part 2..."
    filedata <- readInput inputPath
    problemData <- processInput filedata
    answer <- solve problemData
    putStrLn $ "Sum of output numbers is " ++ (show answer)

  main :: IO ()
  main = do
    part1
    part2
