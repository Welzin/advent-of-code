import System.Environment   
import System.IO
import Data.List
import Data.List.Split
import qualified Data.Set as S
import qualified Data.Map as M

readLines :: FilePath -> IO[(Int, Int)]
readLines f = map ((\x -> (read (x!!0), read (x!!1))) . splitOn ",") . lines <$> readFile f

dirs :: [(Int, Int)]
dirs = [(-1, 0), (0, 1), (1, 0), (0, -1)]

inRange :: (Int, Int) -> Int -> Int -> Bool
inRange (x, y) w h = not(x < 0 || y < 0 || x > w || y > h)

bfs :: [(Int, Int)] -> (Int, Int) -> Int -> Int -> S.Set (Int, Int) -> M.Map (Int, Int) Int -> S.Set (Int, Int) -> M.Map (Int, Int) Int
bfs [] _ _ _ _ dists _ = dists
bfs ((x,y):xs) (ex, ey) w h seen dists walls
  | x == ex && y == ey = dists
  | (x, y) `elem` seen = bfs xs (ex, ey) w h seen dists walls
  | otherwise = bfs updatedQueue (ex, ey) w h (S.insert (x, y) seen) updatedDists walls
  where neighs = [(x + dx, y + dy) | (dx, dy) <- dirs, not((x + dx, y + dy) `elem` walls), inRange (x + dx, y + dy) w h]
        updatedQueue = xs ++ neighs
        updatedDists = foldr (\p acc -> M.insertWith min p (dists M.! (x,y) + 1) acc) dists neighs

part1 :: [(Int, Int)] -> Int -> Int -> Int
part1 walls size restrict = dists M.! (size, size)
  where dists = bfs [(0, 0)] (size, size) size size S.empty (M.singleton (0, 0) 0) $ S.fromList (take restrict walls)

-- This is a kind of bruteforce approach with a dichotomy search. It suffices here, but we could be more efficient
-- by searching the articulation points of the connected component, and getting the first wall that gets on an
-- articulation point.
part2 :: Int -> Int -> [(Int, Int)] -> Int -> (Int, Int)
part2 mn mx walls size
  | mn == mx = walls!!(mn-1)
  | (size, size) `elem` M.keys cc = part2 (md + 1) mx walls size
  | otherwise = part2 mn (md - 1) walls size
  where md = div (mn + mx) 2
        cc = bfs [(0, 0)] (size, size) size size S.empty (M.singleton (0, 0) 0) (S.fromList (take md walls))

main :: IO()
main = do
  args <- getArgs
  contents <- readLines (head args)
  print $ part1 contents (read (args!!1)) (read (args!!2))
  print $ part2 (read (args!!2)) (length contents) contents (read (args!!1))
