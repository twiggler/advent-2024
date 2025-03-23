import Control.Monad ((>=>))
import Control.Monad.State (State, evalState)
import Control.Monad.State qualified as St
import Data.Char (digitToInt)
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq
import System.Environment (getArgs)

data Block = FileId Int | Empty deriving (Show)

data Region = Region
  { offset :: Int,
    size :: Int
  }
  deriving (Show)

type FreeRegion = Region

data FileRegion = FileRegion
  { fileId :: Int,
    region :: Region
  }
  deriving (Show)

readDiskMap :: String -> IO [Int]
readDiskMap = fmap (fmap digitToInt . concat . lines) . readFile

blocks :: [Int] -> [Block]
blocks = zip [0 ..] >=> \(i, n) -> replicate n (if even i then FileId (i `div` 2) else Empty)

regions :: [Int] -> ([FileRegion], [FreeRegion])
regions bs = foldr makeRegion ([], []) (zip3 [0 ..] (scanl (+) 0 bs) bs)
  where
    makeRegion (idx, offset', size') (file, free)
      | even idx = (FileRegion (idx `div` 2) (Region offset' size') : file, free)
      | otherwise = (file, Region offset' size' : free)

compactDisk :: [Block] -> Seq Int
compactDisk = go . Seq.fromList
  where
    go ((FileId h :<| rest) :|> t) = h :<| go (rest :|> t) -- No space to the left
    go ((Empty :<| rest) :|> FileId t) = t :<| go rest -- Compact, move block to the left
    go ((Empty :<| rest) :|> Empty) = go (Empty :<| rest) -- No file to move
    go (FileId h :<| Seq.Empty) = Seq.singleton h -- Last block
    go _ = Seq.empty

defragDisk :: ([FileRegion], [FreeRegion]) -> [FileRegion]
defragDisk (fileRegions, freeRegions) =
  evalState (mapM moveToLeft (reverse fileRegions)) (Seq.fromList freeRegions)
  where
    moveToLeft :: FileRegion -> State (Seq Region) FileRegion
    moveToLeft region'@(FileRegion fid (Region sourceOffset sourceSize)) = do
      emptyRegions <- St.get
      let leftEmptyRegions = Seq.takeWhileL (\er -> offset er < sourceOffset) emptyRegions
          (tooSmall, candidates) = Seq.spanl (\er -> size er < sourceSize) leftEmptyRegions
      case candidates of
        Seq.Empty -> return region'
        (Region destOffset destSize) :<| rest -> do
          St.put $
            if destSize == sourceSize
              then tooSmall <> rest
              else (tooSmall :|> Region (destOffset + sourceSize) (destSize - sourceSize)) <> rest
          return (FileRegion fid (Region destOffset sourceSize))

checksum :: Seq Int -> Int
checksum = sum . Seq.mapWithIndex (*)

checksumRegions :: [FileRegion] -> Int
checksumRegions = sum . concatMap fileRegion
  where
    fileRegion (FileRegion fid (Region o s)) = [fid * offs | offs <- [o .. o + s - 1]]

main :: IO ()
main = do
  (mapFile : _) <- getArgs
  diskMap <- readDiskMap mapFile
  let checksumAfterCompact = checksum $ compactDisk $ blocks diskMap
  let checkSumAfterDefrag = checksumRegions $ defragDisk $ regions diskMap
  putStrLn $ "Disk checksum after compacting equals " ++ show checksumAfterCompact
  putStrLn $ "Disk checksum after defragging equals " ++ show checkSumAfterDefrag
