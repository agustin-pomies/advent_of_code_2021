import Day5
import Test.QuickCheck
import Test.Hspec

-- Generators
genNatural :: Gen Int
genNatural = abs `fmap` (arbitrary :: Gen Int) `suchThat` (>= 0)

genHorizontalSegment :: Gen Segment
genHorizontalSegment = do
  a <- genNatural
  b <- genNatural
  c <- genNatural
  return ((a, c), (b, c))

genVerticalSegment :: Gen Segment
genVerticalSegment = do
  a <- genNatural
  b <- genNatural
  c <- genNatural
  return ((a, b), (a, c))

genGenericSegment :: Gen Segment
genGenericSegment = do
  a <- genNatural
  b <- genNatural
  c <- genNatural
  d <- genNatural
  return ((a, b), (c, d))

-- Props --
-- covers horizontal_segment == zip [a..c] (repeat b)
prop_HorizontalCovers :: Property
prop_HorizontalCovers = forAll genHorizontalSegment $
  \seg@((a, b), (c, _)) -> covers seg == zip [a..c] (repeat b)

-- covers vertical_segment == zip (repeat a) [b..c]
prop_VerticalCovers :: Property
prop_VerticalCovers = forAll genVerticalSegment $
  \seg@((a, b), (_, c)) -> covers seg == zip (repeat a) [b..c]

-- covers diagonal_segment
prop_DiagonalCovers :: Property
prop_DiagonalCovers = forAll genGenericSegment $
  \seg@((a, b), (c, d)) ->
    (a /= c && b /= d) ==>
    if a >= c && b > d
    then covers seg == zip [c..a] [d..a]
    else if a >= c
    then covers seg == zip (reverse [c..a]) [b..d]
    else if a <= c && b < d
    then covers seg == zip [a..c] [b..d]
    else if a <= c
    then covers seg == zip [a..c] (reverse [d..b])
    else False

-- Main Program
main = hspec $ do
  describe "Covers Properties" $
    do it "prop_HorizontalCovers" $ property prop_HorizontalCovers
       it "prop_VerticalCovers"   $ property prop_VerticalCovers
      --  it "prop_DiagonalCovers"   $ property prop_DiagonalCovers
