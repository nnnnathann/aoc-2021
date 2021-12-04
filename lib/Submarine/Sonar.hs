module Submarine.Sonar (
  parseReport,
  SweepReport,
) where

import Data.Windows (window2, window3)
import Text.Megaparsec.Extras (Parser, intP, linesOf)

data SweepReport = SweepReport
  { sweepCountIncreasing :: Int
  , sweepCountIncreasingWindows :: Int
  }

instance Show SweepReport where
  show (SweepReport i w) =
    "Increasing Deltas: " ++ show i ++ "; Increasing Windows: " ++ show w

parseReport :: Parser SweepReport
parseReport = fromList <$> linesOf intP

fromList :: [Int] -> SweepReport
fromList sweep =
  SweepReport
    { sweepCountIncreasing = countIncreasing sweep
    , sweepCountIncreasingWindows = countIncreasingWindows sweep
    }

countIncreasingWindows :: [Int] -> Int
countIncreasingWindows xs = countIncreasing windowSums
 where
  windowSums = map (\(a, b, c) -> a + b + c) (window3 xs)

countIncreasing :: [Int] -> Int
countIncreasing xs = length $ filter (\(x, y) -> y > x) (window2 xs)
