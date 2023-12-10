module Days.Day02 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 parseGame <* endOfInput

parseGame :: Parser Game
parseGame = do
  _ <- string "Game "
  gameId <- decimal
  _ <- string ": "
  gameRounds <- sepBy1 parseGameRound (string "; ") <* endOfLine
  return $ Game {..}

parseGameRound :: Parser GameRound
parseGameRound = do
  sepBy1 (choice [parseRedShown, parseGreenShown, parseBlueShown]) (string ", ")

parseRedShown :: Parser ColorShown
parseRedShown = do
  colorNumber <- decimal
  _ <- string " red"
  return ColorShown {color = Red, ..}

parseGreenShown :: Parser ColorShown
parseGreenShown = do
  colorNumber <- decimal
  _ <- string " green"
  return ColorShown {color = Green, ..}

parseBlueShown :: Parser ColorShown
parseBlueShown = do
  colorNumber <- decimal
  _ <- string " blue"
  return ColorShown {color = Blue, ..}

------------ TYPES ------------
type Input = [Game]

type OutputA = Int

type OutputB = Int

data Color = Red | Green | Blue
  deriving (Show, Eq, Ord)

data ColorShown = ColorShown
  { color :: Color,
    colorNumber :: Int
  }
  deriving (Show)

type GameRound = [ColorShown]

data Game = Game
  { gameRounds :: [GameRound],
    gameId :: Int
  }
  deriving (Show)

------------ PART A ------------

-- Filter out red colors above 12, green colors above 13, blue colors above 14
isValidColorShown :: ColorShown -> Bool
isValidColorShown ColorShown {color = Red, colorNumber} = colorNumber <= 12
isValidColorShown ColorShown {color = Green, colorNumber} = colorNumber <= 13
isValidColorShown ColorShown {color = Blue, colorNumber} = colorNumber <= 14

isValidGameRound :: GameRound -> Bool
isValidGameRound = all isValidColorShown

isValidGame :: Game -> Bool
isValidGame game = all isValidGameRound $ gameRounds game

filterValidGames :: [Game] -> [Game]
filterValidGames = filter isValidGame

partA :: Input -> OutputA
partA input = sum $ map gameId (filterValidGames input)

------------ PART B ------------

toNumbers :: GameRound -> [Int]
toNumbers = map colorNumber

filterColor :: Color -> GameRound -> GameRound
filterColor color = filter (\ColorShown {color = c} -> c == color)

-- Take max of a single color for a game across all rounds
maxColorShown :: Color -> Game -> Int
maxColorShown color game =
  maximum . maximum $
    map toNumbers (filter (not . null) (map (filterColor color) (gameRounds game)))

gamePower :: Game -> Int
gamePower game = do
  product
    [ maxColorShown Red game,
      maxColorShown Green game,
      maxColorShown Blue game
    ]

partB :: Input -> OutputB
partB input = sum $ map gamePower input
