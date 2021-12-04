module Submarine.Controls (
  parsePlan,
  NavCmd (..),
  runPlanv1,
  runPlanv2,
) where

{-
  Controls drives around the submarine.
-}

import qualified Data.Text as Text
import Text.Megaparsec (choice, parseTest)
import Text.Megaparsec.Char (space, string)
import Text.Megaparsec.Extras (Parser, intP, linesOf, spaces)

data NavCmd
  = Up Int
  | Down Int
  | Forward Int
  deriving (Eq, Show)

data Pos = Pos
  { posHorizontal :: Int
  , posDepth :: Int
  , posAim :: Int
  }
  deriving (Show, Eq)

-- Parser for a plan (for example: "up 5\ndown 3" etc)
parsePlan :: Parser [NavCmd]
parsePlan =
  linesOf $
    choice
      [ Forward <$> navCmd "forward"
      , Down <$> navCmd "down"
      , Up <$> navCmd "up"
      ]

-- Generic nav command ("[direction] int")
navCmd :: Text.Text -> Parser Int
navCmd name = string name *> spaces *> intP

-- Returns the end result position (horizontal * depth)
-- of a plan, using the simple movement strategy (no aim)
runPlanv1 :: [NavCmd] -> Int
runPlanv1 = runPlan movev1 posZero

-- Returns the end result position (horizontal * depth)
-- of a plan, using the aim strategy
runPlanv2 :: [NavCmd] -> Int
runPlanv2 = runPlan movev2 posZero

runPlan :: (Pos -> NavCmd -> Pos) -> Pos -> [NavCmd] -> Int
runPlan f init plan = value (foldl f init plan)
 where
  value = uncurry (*) . posTuple

posZero :: Pos
posZero = Pos 0 0 0

posTuple :: Pos -> (Int, Int)
posTuple p = (posHorizontal p, posDepth p)

-- Move strategy 1 (simple, no aim)
movev1 :: Pos -> NavCmd -> Pos
movev1 p cmd = case cmd of
  Up n -> p{posDepth = posDepth p - n}
  Down n -> p{posDepth = posDepth p + n}
  Forward n -> p{posHorizontal = posHorizontal p + n}

-- Move strategy 2 (with aim)
movev2 :: Pos -> NavCmd -> Pos
movev2 p cmd = case cmd of
  Up n -> p{posAim = posAim p - n}
  Down n -> p{posAim = posAim p + n}
  Forward n ->
    p
      { posHorizontal = posHorizontal p + n
      , posDepth = posDepth p + (n * posAim p)
      }
