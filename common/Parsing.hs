module Parsing
  ( parseFileWith,
    eol,
    number,
    readLines,
    signedNumber,
    grid,
  )
where

import Data.Char (isDigit)
import Data.Functor (($>))
import Text.ParserCombinators.ReadP (ReadP, (+++))
import Text.ParserCombinators.ReadP qualified as P 

readLines :: String -> IO [String]
readLines = fmap lines . readFile

parseFileWith :: ReadP p -> String -> IO p
parseFileWith parser filename = fst . last . P.readP_to_S parser <$> readFile filename

digits :: ReadP String
digits = P.munch1 isDigit

number :: (Integral a, Read a) => ReadP a
number = read <$> digits

signedNumber :: (Integral a, Read a) => ReadP a
signedNumber = read <$> ((:) <$> P.char '-' <*> digits) +++ digits

eol :: ReadP ()
eol = P.char '\n' $> ()

grid :: ReadP c -> ReadP (Int, [[c]])
grid cellParser = do
  firstRow <- P.manyTill cellParser eol
  let width = length firstRow
  rest <- P.endBy (P.count width cellParser) eol 
  return (width, firstRow : rest)
