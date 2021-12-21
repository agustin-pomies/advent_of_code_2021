import Data.List.Split (splitOn)
import Data.Map (fromList)

import Lanternfish hiding (main)

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Hspec

type Lanternfish = Int

-- Example --
testPath :: String
testPath = basePath ++ "examples/6.txt"

readExample :: IO [LanternfishGroup]
readExample = do
  content <- readFile testPath
  return $ (frequencies . (map read) . (splitOn ",") . head . lines) content

-- Generators --
genNatural :: Gen Int
genNatural = abs `fmap` (arbitrary :: Gen Int) `suchThat` (>= 0)

genLanternfish :: Gen Lanternfish
genLanternfish = chooseInt (0,8)

genLanternfishGroup :: Gen LanternfishGroup
genLanternfishGroup = do
  timer <- genLanternfish
  count <- genNatural
  return (timer, count)

genLanternfishPopulation :: Gen [LanternfishGroup]
genLanternfishPopulation = listOf1 genLanternfishGroup

-- Props --
-- Pure modulus is not a valid operation since a new fish timer can be greater than 6
-- A new fish is created ONLY when a fish counter resets (goes from 0 to 6)

prop_Example18 :: Property
prop_Example18 = monadicIO $ do
  population <- run $ readExample
  assert $ length (simulate (fromList population) 18) == 26

prop_Example80 :: Property
prop_Example80 = monadicIO $ do
  population <- run $ readExample
  assert $ length (simulate (fromList population) 80) == 5934

-- Main Test Module --
main = hspec $ do
  describe "Lanternfish - Part 1" $
    do
      it "prop_Example18" $ property prop_Example18
      it "prop_Example80" $ property prop_Example80
