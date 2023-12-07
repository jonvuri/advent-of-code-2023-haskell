module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Char (digitToInt)
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
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 rowParser

rowParser :: Parser [Token]
rowParser = many1 (choice [digitNum, spelledNum, nonDigitNum]) <* endOfLine

digitNum :: Parser Token
digitNum = DigitChar <$> digit

nonDigitNum :: Parser Token
nonDigitNum = do
  _ <- letter
  return NonDigit

-- one
spelledOne :: Parser Token
spelledOne = do
  c1 <- char 'o'
  (c2, c3) <- lookAhead $ do
    c2 <- char 'n'
    c3 <- char 'e'
    return (c2, c3)
  return $ case (c1, c2, c3) of
    ('o', 'n', 'e') -> SpelledDigit '1'
    _ -> NonDigit

-- two
spelledTwo :: Parser Token
spelledTwo = do
  c1 <- char 't'
  (c2, c3) <- lookAhead $ do
    c2 <- char 'w'
    c3 <- char 'o'
    return (c2, c3)
  return $ case (c1, c2, c3) of
    ('t', 'w', 'o') -> SpelledDigit '2'
    _ -> NonDigit

-- three
spelledThree :: Parser Token
spelledThree = do
  c1 <- char 't'
  (c2, c3, c4, c5) <- lookAhead $ do
    c2 <- char 'h'
    c3 <- char 'r'
    c4 <- char 'e'
    c5 <- char 'e'
    return (c2, c3, c4, c5)
  return $ case (c1, c2, c3, c4, c5) of
    ('t', 'h', 'r', 'e', 'e') -> SpelledDigit '3'
    _ -> NonDigit

-- four
spelledFour :: Parser Token
spelledFour = do
  c1 <- char 'f'
  (c2, c3, c4) <- lookAhead $ do
    c2 <- char 'o'
    c3 <- char 'u'
    c4 <- char 'r'
    return (c2, c3, c4)
  return $ case (c1, c2, c3, c4) of
    ('f', 'o', 'u', 'r') -> SpelledDigit '4'
    _ -> NonDigit

-- five
spelledFive :: Parser Token
spelledFive = do
  c1 <- char 'f'
  (c2, c3, c4) <- lookAhead $ do
    c2 <- char 'i'
    c3 <- char 'v'
    c4 <- char 'e'
    return (c2, c3, c4)
  return $ case (c1, c2, c3, c4) of
    ('f', 'i', 'v', 'e') -> SpelledDigit '5'
    _ -> NonDigit

-- six
spelledSix :: Parser Token
spelledSix = do
  c1 <- char 's'
  (c2, c3) <- lookAhead $ do
    c2 <- char 'i'
    c3 <- char 'x'
    return (c2, c3)
  return $ case (c1, c2, c3) of
    ('s', 'i', 'x') -> SpelledDigit '6'
    _ -> NonDigit

-- seven
spelledSeven :: Parser Token
spelledSeven = do
  c1 <- char 's'
  (c2, c3, c4, c5) <- lookAhead $ do
    c2 <- char 'e'
    c3 <- char 'v'
    c4 <- char 'e'
    c5 <- char 'n'
    return (c2, c3, c4, c5)
  return $ case (c1, c2, c3, c4, c5) of
    ('s', 'e', 'v', 'e', 'n') -> SpelledDigit '7'
    _ -> NonDigit

-- eight
spelledEight :: Parser Token
spelledEight = do
  c1 <- char 'e'
  (c2, c3, c4, c5) <- lookAhead $ do
    c2 <- char 'i'
    c3 <- char 'g'
    c4 <- char 'h'
    c5 <- char 't'
    return (c2, c3, c4, c5)
  return $ case (c1, c2, c3, c4, c5) of
    ('e', 'i', 'g', 'h', 't') -> SpelledDigit '8'
    _ -> NonDigit

-- nine
spelledNine :: Parser Token
spelledNine = do
  c1 <- char 'n'
  (c2, c3, c4) <- lookAhead $ do
    c2 <- char 'i'
    c3 <- char 'n'
    c4 <- char 'e'
    return (c2, c3, c4)
  return $ case (c1, c2, c3, c4) of
    ('n', 'i', 'n', 'e') -> SpelledDigit '9'
    _ -> NonDigit

spelledNum :: Parser Token
spelledNum =
  choice
    [ spelledOne,
      spelledTwo,
      spelledThree,
      spelledFour,
      spelledFive,
      spelledSix,
      spelledSeven,
      spelledEight,
      spelledNine
    ]

------------ TYPES ------------
type Input = [[Token]]

type OutputA = Int

type OutputB = Int

-- Parser spits out:
--   - DigitChar for input like '1'
--   - SpelledDigit for input like 'one', but only at the first character
--   - NonDigit for input like 'one' at second and third characters, or any other input
data Token = DigitChar Char | SpelledDigit Char | NonDigit deriving (Show, Eq)

------------ PART A ------------

tokenToInt :: Token -> Int
tokenToInt (DigitChar c) = digitToInt c
tokenToInt (SpelledDigit c) = digitToInt c

lineValue :: [Token] -> Int
lineValue xs = tokenToInt (head xs) * 10 + tokenToInt (last xs)

onlyDigits :: [Token] -> [Token]
onlyDigits = filter onlyDigits
  where
    onlyDigits (DigitChar _) = True
    onlyDigits _ = False

partA :: Input -> OutputA
partA input = sum $ map (lineValue . onlyDigits) input

------------ PART B ------------

anyDigits :: [Token] -> [Token]
anyDigits = filter anyDigits
  where
    anyDigits (DigitChar _) = True
    anyDigits (SpelledDigit _) = True
    anyDigits _ = False

partB :: Input -> OutputB
partB input = sum $ map (lineValue . anyDigits) input
