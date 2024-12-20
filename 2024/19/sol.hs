import System.Environment   
import System.IO
import Data.List
import Data.List.Split
import qualified Data.Map as M

readLines :: FilePath -> IO([String], [String])
readLines f = (\l -> (splitOn ", " (l!!0), lines (l!!1))) . splitOn "\n\n" <$> readFile f

dp :: [String] -> String -> String -> M.Map String Int -> Int
dp _ [] w map_ = M.findWithDefault 0 w map_
dp patterns (c:cs) w map_
  | w `elem` M.keys map_ = dp patterns cs (w ++ [c]) updatedMap
  | otherwise = dp patterns cs (w ++ [c]) map_
  where towels = map_ M.! w
        toUpdate = [w ++ v | v <- patterns, v `isPrefixOf` (c:cs)]
        updatedMap = foldr (\v acc -> M.insertWith (+) v towels acc) map_ toUpdate

part1 :: ([String], [String]) -> Int
part1 (patterns, xs) = foldr (\x acc -> fromEnum (dp patterns x "" (M.singleton "" 1) > 0) + acc) 0 xs

part2 :: ([String], [String]) -> Int
part2 (patterns, xs) = foldr (\x acc -> dp patterns x "" (M.singleton "" 1) + acc) 0 xs

main :: IO()
main = do
  args <- getArgs
  contents <- readLines (head args)
  print $ part1 contents 
  print $ part2 contents
