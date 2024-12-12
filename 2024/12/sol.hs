import System.Environment   
import System.IO()
import Data.Array
import Data.Set
import qualified Data.Map as M
import qualified Data.List as L
import Debug.Trace

data Dir = North | South | East | West deriving (Enum, Eq, Ord, Show)

readLine :: FilePath -> IO[String]
readLine f = lines <$> readFile f

stringListArray :: [String] -> Array Int (Array Int Char)
stringListArray s = listArray (0, length s - 1) (Prelude.map (\x -> listArray (0, length x - 1) x) s)

inRange :: Int -> Int -> (Int, Int) -> Bool
inRange w h (x,y) = not(x < 0 || y < 0 || x >= h || y >= w)

sameChar :: Array Int (Array Int Char) -> (Int, Int) -> (Int, Int) -> Bool
sameChar map_ (x, y) (a, b) = (map_!a!b) == (map_!x!y)

neighbourhood :: Int -> Int -> [(Int, Int)]
neighbourhood x y = [(x + a, y + b) | (a, b) <- [(0, -1), (0, 1), (-1, 0), (1, 0)]]

neighbours :: Array Int (Array Int Char) -> Int -> Int -> (Int, Int) -> [(Int, Int)]
neighbours map_ w h (x, y) = Prelude.filter (\(a,b) -> Main.inRange w h (a,b) && sameChar map_ (x, y) (a, b)) $ neighbourhood x y

dfs :: Array Int (Array Int Char) -> Int -> Int -> (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
dfs map_ w h (x, y) visited
  | (x, y) `elem` visited = visited
  | otherwise = Prelude.foldr (dfs map_ w h) (insert (x, y) visited) neighs
  where neighs = neighbours map_ w h (x,y)

getCC :: Array Int (Array Int Char) -> Int -> Int -> Int -> Int -> Set (Int, Int) -> [[(Int, Int)]]
getCC map_ x y w h visited
  | x == h = []
  | y == w = getCC map_ (x + 1) 0 w h visited
  | (x,y) `elem` visited = getCC map_ x (y + 1) w h visited
  | otherwise = toList vis : getCC map_ x (y + 1) w h (union visited vis)
  where vis = dfs map_ w h (x,y) empty

inNeighbourhood :: Int -> Int -> (Int, Int) -> Bool
inNeighbourhood x y z = z `elem` neighbourhood x y

perimeter :: [(Int, Int)] -> [(Int, Int)] -> Int
perimeter [] _ = 0
perimeter ((x1,x2):xs) witness = 4 - length (Prelude.filter (inNeighbourhood x1 x2) witness) + perimeter xs witness

part1 :: [[(Int, Int)]] -> Int
part1 = Prelude.foldr (\l acc -> acc + length l * perimeter l l) 0 

removeWithBool :: (Int, Int) -> [(Int, Int)] -> ([(Int, Int)], Bool)
removeWithBool _ [] = ([], False)
removeWithBool p (q:qs)
  | p == q = (qs, True)
  | otherwise = (q:xs, bres)
  where (xs, bres) = removeWithBool p qs

updateAux :: Dir -> Int -> Int -> M.Map Dir [(Int, Int)] -> (M.Map Dir [(Int, Int)], Bool)
updateAux dir x' y' m =
  let (newList, status) = removeWithBool (x', y') (m M.! dir) in
    (M.adjust (\_ -> newList) dir m, status)

updateDir :: Dir -> Int -> Int -> M.Map Dir [(Int, Int)] -> M.Map Dir [(Int, Int)]
updateDir North x y m
  | hasRemoved = map_
  | otherwise = M.adjust ((x,y):) North map_
  where (map_, hasRemoved) = updateAux South (x-1) y m
updateDir South x y m
  | hasRemoved = map_
  | otherwise = M.adjust ((x,y):) South map_
  where (map_, hasRemoved) = updateAux North (x+1) y m
updateDir West x y m
  | hasRemoved = map_
  | otherwise = M.adjust ((x,y):) West map_
  where (map_, hasRemoved) = updateAux East x (y-1) m
updateDir East x y m
  | hasRemoved = map_
  | otherwise = M.adjust ((x,y):) East map_
  where (map_, hasRemoved) = updateAux West x (y+1) m

updateWalls :: Int -> Int -> M.Map Dir [(Int, Int)] -> [Dir] -> M.Map Dir [(Int, Int)]
updateWalls _ _ m [] = m
updateWalls x y m (dir:dirs) = updateWalls x y (updateDir dir x y m) dirs

numberOfSides :: (Int, Int) -> [(Int, Int)] -> Int 
numberOfSides _ [] = 0
numberOfSides (x,y) ((x',y'):xs)
  | diff >= 2 = 1 + recVal
  | otherwise = recVal
  where recVal = numberOfSides (x',y') xs
        diff = abs (x - x') + abs (y - y')

sides :: [(Int, Int)] -> M.Map Dir [(Int, Int)] -> Int
sides [] m = numberOfSides (-1, -1) (L.sort (m M.! North)) + numberOfSides (-1, -1) (L.sort (m M.! South)) +
             numberOfSides (-1, -1) (L.sort . Prelude.map swap $ (m M.! East)) + numberOfSides (-1, -1) (L.sort . Prelude.map swap $ (m M.! West))
  where swap (a,b) = (b,a)
        -- we need to swap for east and west, otherwise they are sorted by 1st element, breaking our invariant of numberOfSides.
sides ((x,y):xs) m = sides xs map_
  where map_ = updateWalls x y m [North, East, South, West]

part2 :: [[(Int, Int)]] -> Int
part2 = Prelude.foldr (\l acc -> acc + length l * sides l emptyWithDirs) 0
  where emptyWithDirs = M.fromList [(North, []), (East, []), (South, []), (West, [])]

run :: Array Int (Array Int Char) -> IO()
run map_ =
  let w = length (map_!0)
      h = length map_
      connectedComponents = getCC map_ 0 0 w h empty in
  do
    print $ part1 connectedComponents
    print $ part2 connectedComponents

main :: IO()
main = do
  args <- getArgs
  contents <- readLine (head args)
  run $ stringListArray contents
