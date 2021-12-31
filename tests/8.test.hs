import Day8 hiding (main)

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Hspec

-- Example --
testPath :: String
testPath = basePath ++ "examples/8.txt"

-- Generators --
smallExample :: Entry
smallExample = let sample = ["acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"]
                   output = ["cdfeb", "fcadb", "cdfeb", "cdbaf"]
               in (sample, output)

-- Props --
prop_smallExample :: Bool
prop_smallExample = entryOutput smallExample == 5353

prop_largerExample :: Property
prop_largerExample = monadicIO $ do
  filedata <- run $ readInput testPath
  noisyNotes <- run $ processInput filedata
  assert $ (map entryOutput noisyNotes) == [8394, 9781, 1197, 9361, 4873, 8418, 4548, 1625, 8717, 4315]

-- Main Test Module --
main = hspec $ do
  describe "Seven Segment Search - Part 2" $
    do
      it "prop_smallExample" $ property prop_smallExample
      it "prop_largerExample" $ property prop_largerExample
