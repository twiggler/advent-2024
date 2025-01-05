module Parsing (
    parseFileWith
) where
import Text.ParserCombinators.ReadP

parseFileWith :: ReadP p -> String -> IO p
parseFileWith parser filename = fst . last . readP_to_S parser <$> readFile filename
