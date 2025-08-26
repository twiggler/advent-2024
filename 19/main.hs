import Algorithm.Search (dfs)
import Data.ByteString.Char8 qualified as B (null, pack)
import Data.Maybe qualified as M (mapMaybe)
import Data.Trie qualified as T (fromList, matches)
import Parsing (eol, parseFileWith)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP, endBy1, eof, munch1, sepBy1, string)

towels :: ReadP ([String], [String])
towels = (,) <$> patterns <* eol <*> designs <* eof
  where
    stripe = munch1 (`elem` "wubrg")
    patterns = sepBy1 stripe (string ", ") <* eol
    designs = endBy1 stripe eol

solve1 :: [String] -> [String] -> Int
solve1 patterns designs =
  length $ M.mapMaybe (dfs next B.null . B.pack) designs
  where
    patternTrie = T.fromList [(B.pack p, ()) | p <- patterns]
    next state = [remainder | (_, _, remainder) <- T.matches patternTrie state]

main :: IO ()
main = do
  (towelFile : _) <- getArgs
  (patterns, designs) <- parseFileWith towels towelFile
  let possibleDesigns = solve1 patterns designs
  putStrLn $ "Number of possible designs: " ++ show possibleDesigns
