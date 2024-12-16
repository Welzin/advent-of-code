import System.Environment   
import System.IO
import Data.Array
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List

readLine :: FilePath -> IO[String]
readLine f = lines <$> readFile f

stringListArray :: [String] -> Array Int (Array Int Char)
stringListArray s = listArray (0, length s - 1) (map (\x -> listArray (0, length x - 1) x) s)

dirs :: [(Int, Int)]
dirs = [(-1, 0), (0, 1), (1, 0), (0, -1)]

dijkstra :: Array Int (Array Int Char) -> S.Set (Int, (Int, Int, Int, Int)) -> M.Map (Int, Int, Int, Int) Int -> M.Map (Int, Int, Int, Int) Int
dijkstra map_ queue dist =
  case S.minView queue of
    Nothing -> dist
    Just ((cost, (dx, dy, x, y)), nqueue)
      | M.findWithDefault (maxBound :: Int) (dx, dy, x, y) dist < cost -> dijkstra map_ nqueue dist
      | otherwise -> dijkstra map_ queueWithRotationsAndDir (M.insertWith min (dx, dy, x, y) cost dist)
      where rotations = [(cost + 1000, (a, b, x, y)) | (a, b) <- dirs, M.findWithDefault (maxBound :: Int) (a, b, x, y) dist > cost + 1000]
            nextDir = if map_!(x+dx)!(y+dy) /= '#' then S.insert (cost + 1, (dx,dy,x+dx,y+dy)) (S.fromList rotations) else S.fromList rotations
            queueWithRotationsAndDir = S.union nqueue nextDir

part1 :: Array Int (Array Int Char) -> (Int, Int) -> (Int, Int) -> Int
part1 map_ (sx,sy) (ex,ey) = minimum [M.findWithDefault (maxBound :: Int) (dx, dy, ex, ey) dists | (dx, dy) <- dirs]
  where dists = dijkstra map_ (S.singleton (0, (0, 1, sx, sy))) M.empty

part2 :: Array Int (Array Int Char) -> (Int, Int) -> (Int, Int) -> Int
part2 map_ (sx,sy) (ex,ey) = length filteredVertices
  where dists1 = dijkstra map_ (S.singleton (0, (0, 1, sx, sy))) M.empty
        dists2 = dijkstra map_ (S.fromList startFromEd) M.empty
        best = minimum [M.findWithDefault (maxBound :: Int) (dx, dy, ex, ey) dists1 | (dx, dy) <- dirs]
        startFromEd = [(0, (dx, dy, ex, ey)) | (dx, dy) <- dirs, map_!(ex + dx)!(ey + dy) /= '#']
        allKeys = S.toList (S.union (S.fromList (M.keys dists1)) (S.fromList (M.keys dists2)))
        reverseDir (dx,dy,x,y) = (-dx,-dy,x,y)
        vertices = filter (\u -> M.findWithDefault (maxBound :: Int) u dists1 + M.findWithDefault (maxBound :: Int) (reverseDir u) dists2 == best) allKeys
        filteredVertices = S.fromList [(x, y) | (_,_,x,y) <- vertices]

run :: Array Int (Array Int Char) -> IO()
run map_ = do
  print $ part1 map_ start end
  print $ part2 map_ start end
  where start = (h - 2, 1)
        end   = (1, w - 2)
        h     = length map_
        w     = length (map_!0)

main :: IO()
main = do
  args <- getArgs
  contents <- readLine (head args)
  run $ stringListArray contents
