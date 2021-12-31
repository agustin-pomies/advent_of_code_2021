import Day9 hiding (main)

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Hspec

-- Example --
testPath :: String
testPath = basePath ++ "examples/9.txt"

-- Generators --
-- Add more generators...

-- Props --
prop_example :: Property
prop_example = monadicIO $ do
  filedata <- run $ readInput testPath
  problemData <- run $ processInput filedata
  answer <- run $ solve problemData
  assert $ answer == 15

-- Main Test Module --
main = hspec $ do
  describe "Smoke Basin - Part 1" $
    do
      it "prop_example" $ property prop_example
