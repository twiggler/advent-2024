module Parsing
  ( parseFileWith,
    eol,
  )
where

import Data.Functor
import Text.ParserCombinators.ReadP

parseFileWith :: ReadP p -> String -> IO p
parseFileWith parser filename = fst . last . readP_to_S parser <$> readFile filename

eol :: ReadP ()
eol = char '\n' $> ()
