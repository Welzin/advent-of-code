import System.Environment   
import System.IO
import Data.List
import Data.List.Split
import qualified Data.Map as M

readLines :: FilePath -> IO([String], [String])
readLines f = (\l -> (splitOn ", " (l!!0), lines (l!!1))) . splitOn "\n\n" <$> readFile f

dp :: [String] -> String -> String -> M.Map String Int -> Int
dp _ [] w map_ = M.findWithDefault (-1) w map_
dp patterns (c:cs) w map_
  | w `elem` M.keys map_ = dp patterns cs (w ++ [c]) updatedMap
  | otherwise = dp patterns cs (w ++ [c]) map_
  where towels = map_ M.! w
        toUpdate = [w ++ v | v <- patterns, v `isPrefixOf` (c:cs)]
        updatedMap = foldr (\v acc -> M.insertWith (+) v towels acc) map_ toUpdate

part1 :: ([String], [String]) -> Int
part1 (_, []) = 0
part1 (patterns, x:xs) = fromEnum (dp patterns x "" (M.singleton "" 0) /= -1) + part1 (patterns, xs)

part2 :: ([String], [String]) -> Int
part2 (_, []) = 0
part2 (patterns, x:xs) = max 0 (dp patterns x "" (M.singleton "" 1)) + part2 (patterns, xs)

main :: IO()
main = do
  args <- getArgs
  contents <- readLines (head args)
  print $ part1 contents 
  print $ part2 contents
