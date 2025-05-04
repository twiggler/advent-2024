module Parsing
  ( parseFileWith,
    eol,
    number,
    readLines
  )
where

import Data.Functor
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

readLines :: String -> IO [String]
readLines = fmap lines . readFile

parseFileWith :: ReadP p -> String -> IO p
parseFileWith parser filename = fst . last . readP_to_S parser <$> readFile filename

number :: (Integral a, Read a) => ReadP a
number = read <$> munch1 isDigit

eol :: ReadP ()
eol = char '\n' $> ()
