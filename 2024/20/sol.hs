import System.Environment   
import System.IO
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Array

readLines :: FilePath -> IO[String]
readLines f = lines <$> readFile f

stringListArray :: [String] -> Array Int (Array Int Char)
stringListArray s = listArray (0, length s - 1) (map (\x -> listArray (0, length x - 1) x) s)

charPos :: Array Int (Array Int Char) -> Int -> Int -> Int -> Int -> Char -> (Int, Int)
charPos map_ x y w h c
  | x == h = (-1, -1)
  | y == w = charPos map_ (x+1) 0 w h c
  | map_!x!y == c = (x, y)
  | otherwise = charPos map_ x (y+1) w h c

neighbours :: Array Int (Array Int Char) -> (Int, Int) -> [(Int, Int)]
neighbours map_ (x, y) = filter (\(a,b) -> map_!a!b /= '#')
                         [(x + a, y + b) | (a, b) <- [(0, -1), (0, 1), (-1, 0), (1, 0)]]

bfs :: Array Int (Array Int Char) -> [(Int, (Int, Int))] -> M.Map (Int, Int) Int -> S.Set (Int, Int) -> M.Map (Int, Int) Int
bfs _ [] dists _ = dists
bfs map_ ((c, (x, y)):xs) dists seen
  | (x, y) `elem` seen = bfs map_ xs dists seen
  | otherwise = bfs map_ (xs ++ neighs) updatedDists (S.insert (x, y) seen)
  where neighs = [(c + 1, p) | p <- neighbours map_ (x, y)]
        updatedDists = M.insert (x, y) c dists

shortcuts :: [(Int, Int)] -> M.Map (Int, Int) Int -> Int -> M.Map Int Int
shortcuts nodes dists ε = M.map (`div` 2) acc
  where acc = foldr (\(d, (p, q)) vMap -> M.insertWith (+) (abs (dists M.! q - dists M.! p) - d) 1 vMap) M.empty pairs
        pairs = [(abs (x - x') + abs (y - y'), ((x, y), (x', y'))) | (x, y) <- nodes, (x', y') <- nodes, abs (x - x') + abs (y - y') <= ε]

part1 :: Array Int (Array Int Char) -> (Int, Int) -> Int
part1 map_ st = foldr (\k acc -> acc + save M.! k) 0 $ filter (>=100) (M.keys save)
  where dists = bfs map_ [(0, st)] M.empty S.empty
        save = shortcuts keys dists 2
        keys = M.keys dists

part2 :: Array Int (Array Int Char) -> (Int, Int) -> Int
part2 map_ st = foldr (\k acc -> acc + save M.! k) 0 $ filter (>=100) (M.keys save)
  where dists = bfs map_ [(0, st)] M.empty S.empty
        save = shortcuts keys dists 20
        keys = M.keys dists

run :: Array Int (Array Int Char) -> IO()
run map_ = do
  print $ part1 map_ st
  print $ part2 map_ st
  where st = charPos map_ 1 1 w h 'S'
        w = length (map_!0)
        h = length map_

main :: IO()
main = do
  args <- getArgs
  contents <- readLines (head args)
  run (stringListArray contents)
