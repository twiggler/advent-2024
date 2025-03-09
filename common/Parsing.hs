module Parsing
  ( parseFileWith,
    eol,
    number
  )
where

import Data.Functor
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

parseFileWith :: ReadP p -> String -> IO p
parseFileWith parser filename = fst . last . readP_to_S parser <$> readFile filename

number :: ReadP Int
number = read <$> munch1 isDigit

eol :: ReadP ()
eol = char '\n' $> ()
