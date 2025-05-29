module Parsing
  ( parseFileWith,
    eol,
    number,
    readLines,
    signedNumber
  )
where

import Data.Char (isDigit)
import Data.Functor (($>))
import Text.ParserCombinators.ReadP (ReadP, char, munch1, readP_to_S, (+++))

readLines :: String -> IO [String]
readLines = fmap lines . readFile

parseFileWith :: ReadP p -> String -> IO p
parseFileWith parser filename = fst . last . readP_to_S parser <$> readFile filename

digits :: ReadP String
digits = munch1 isDigit

number :: (Integral a, Read a) => ReadP a
number = read <$> digits

signedNumber :: (Integral a, Read a) => ReadP a
signedNumber =  read <$> ((:) <$> char '-' <*> digits) +++ digits

eol :: ReadP ()
eol = char '\n' $> ()
