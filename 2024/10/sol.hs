import System.Environment   
import System.IO()
import Data.Array
import Data.Char
import qualified Data.Set as S

readLine :: FilePath -> IO[String]
readLine f = lines <$> readFile f

stringListArray :: [String] -> Array Int (Array Int Char)
stringListArray s = listArray (0, length s - 1) (map (\x -> listArray (0, length x - 1) x) s)

start :: Array Int (Array Int Char) -> Int -> Int -> Int -> Int -> [(Int, Int)]
start map_ x y w h
  | x == h = []
  | y == w = start map_ (x+1) 0 w h
  | map_!x!y == '0' = (x,y):start map_ x (y+1) w h
  | otherwise = start map_ x (y+1) w h

inRange :: Int -> Int -> (Int, Int) -> Bool
inRange w h (x,y) = not(x < 0 || y < 0 || x >= h || y >= w)

haveDistOne :: Array Int (Array Int Char) -> (Int, Int) -> (Int, Int) -> Bool
haveDistOne map_ (x, y) (a, b) = (digitToInt $ map_!a!b) - (digitToInt $ map_!x!y) == 1

neighbours :: Array Int (Array Int Char) -> Int -> Int -> (Int, Int) -> [(Int, Int)]
neighbours map_ w h (x, y) = filter (\(a,b) -> Main.inRange w h (a,b) && haveDistOne map_ (x, y) (a, b))
                             [(x + a, y + b) | (a, b) <- [(0, -1), (0, 1), (-1, 0), (1, 0)]]

dfsAux :: Array Int (Array Int Char) -> Int -> Int -> (Int, Int) -> S.Set (Int, Int) -> (Int, S.Set (Int, Int))
dfsAux map_ w h (x, y) visited
  | (x, y) `elem` visited = (0, visited)
  | map_!x!y == '9' = (1, S.insert (x, y) visited)
  | otherwise = foldr addDfsAux (0, S.insert (x, y) visited) neighs
  where addDfsAux p (x, nvisited) = let (y,visited') = dfsAux map_ w h p nvisited in (x+y,visited')
        neighs = neighbours map_ w h (x,y)

dfs :: Array Int (Array Int Char) -> Int -> Int -> (Int, Int) -> Int
dfs map_ w h (x,y) = fst $ dfsAux map_ w h (x, y) S.empty

part1 :: Array Int (Array Int Char) -> [(Int, Int)] -> Int -> Int -> Int
part1 map_ xs w h = sum . map (dfs map_ w h) $ xs

dfs' :: Array Int (Array Int Char) -> Int -> Int -> (Int, Int) -> Int
dfs' map_ w h (x, y)
  | map_!x!y == '9' = 1
  | otherwise = sum . map (dfs' map_ w h) $ neighbours map_ w h (x,y)

part2 :: Array Int (Array Int Char) -> [(Int, Int)] -> Int -> Int -> Int
part2 map_ xs w h = sum . map (dfs' map_ w h) $ xs

run :: Array Int (Array Int Char) -> IO()
run map_ =
  let w = length (map_!0)
      h = length map_
      startingPoints = start map_ 0 0 w h in
  do
    print $ part1 map_ startingPoints w h
    print $ part2 map_ startingPoints w h

main :: IO()
main = do
  args <- getArgs
  contents <- readLine (head args)
  run $ stringListArray contents
