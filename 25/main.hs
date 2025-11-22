import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Either (partitionEithers)
import Data.Functor (($>))
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Parsing (eol, parseFileWith)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as P

type Schematic = [[Bool]]

newtype Lock = Lock [Int] deriving (Show)

newtype Key = Key [Int] deriving (Show)

maxHeight :: Int
maxHeight = 6

readSchematics :: ReadP [Schematic]
readSchematics = P.sepBy schematic eol <* P.eof
  where
    schematic = P.count (maxHeight + 1) row
    row = P.count 5 cell <* eol
    cell = (P.char '#' $> True) <|> (P.char '.' $> False)

mkKeysAndLocks :: [Schematic] -> Maybe ([Lock], [Key])
mkKeysAndLocks =
  fmap partitionEithers . traverse mkLockOrKey
  where
    mkLockOrKey = liftA2 (<|>) isLock isKey
    heights (x : xs) =
      guard (and x) $> (fromMaybe maxHeight . L.findIndex not <$> L.transpose xs)
    heights [] = Nothing

    isLock = fmap (Left . Lock) . heights
    isKey = fmap (Right . Key) . heights . reverse

solve1 :: [Schematic] -> Maybe Int
solve1 schematics = do
  (locks, keys) <- mkKeysAndLocks schematics
  let combinedHeights = [zipWith (+) hl hk | Lock hl <- locks, Key hk <- keys]
  return $ length $ filter (all (< maxHeight)) combinedHeights

main :: IO ()
main = do
  (schematicsFile : _) <- getArgs
  schematics <- parseFileWith readSchematics schematicsFile

  case solve1 schematics of
    Just result -> putStrLn $ "Number of lock / key pairs that fit together is: " ++ show result
    Nothing -> putStrLn "No solution found"
