module Submarine.Controls (
  parsePlan,
  NavCmd (..),
  runPlanv1,
  runPlanv2,
) where

import qualified Data.Text as Text
import Text.Megaparsec (choice, parseTest)
import Text.Megaparsec.Char (space, string)
import Text.Megaparsec.Extras (Parser, intP, linesOf, spaces)

data NavCmd
  = Up Int
  | Down Int
  | Forward Int
  deriving (Eq, Show)

-- Nav Commands

parsePlan :: Parser [NavCmd]
parsePlan =
  linesOf $
    choice
      [ forwardP
      , downP
      , upP
      ]

forwardP :: Parser NavCmd
forwardP = Forward <$> navCmd "forward"

downP :: Parser NavCmd
downP = Down <$> navCmd "down"

upP :: Parser NavCmd
upP = Up <$> navCmd "up"

navCmd :: Text.Text -> Parser Int
navCmd name = string name *> spaces *> intP

runPlanv1 :: [NavCmd] -> Int
runPlanv1 = runPlan movev1 posZero

runPlanv2 :: [NavCmd] -> Int
runPlanv2 = runPlan movev2 posZero

runPlan :: (Pos -> NavCmd -> Pos) -> Pos -> [NavCmd] -> Int
runPlan f init plan = value (foldl f init plan)
 where
  value = uncurry (*) . posTuple

data Pos = Pos
  { posHorizontal :: Int
  , posDepth :: Int
  , posAim :: Int
  }
  deriving (Show, Eq)

posZero :: Pos
posZero = Pos 0 0 0

type Plan = [NavCmd]

posTuple :: Pos -> (Int, Int)
posTuple p = (posHorizontal p, posDepth p)

movev1 :: Pos -> NavCmd -> Pos
movev1 p (Forward n) =
  Pos
    { posHorizontal = posHorizontal p + n
    , posDepth = posDepth p
    , posAim = posAim p
    }
movev1 p (Up n) =
  Pos
    { posHorizontal = posHorizontal p
    , posDepth = posDepth p - n
    , posAim = posAim p
    }
movev1 p (Down n) =
  Pos
    { posHorizontal = posHorizontal p
    , posDepth = posDepth p + n
    , posAim = posAim p
    }

movev2 :: Pos -> NavCmd -> Pos
movev2 p (Forward n) =
  Pos
    { posHorizontal = posHorizontal p + n
    , posDepth = posDepth p + (n * posAim p)
    , posAim = posAim p
    }
movev2 p (Up n) =
  Pos
    { posHorizontal = posHorizontal p
    , posDepth = posDepth p
    , posAim = posAim p - n
    }
movev2 p (Down n) =
  Pos
    { posHorizontal = posHorizontal p
    , posDepth = posDepth p
    , posAim = posAim p + n
    }