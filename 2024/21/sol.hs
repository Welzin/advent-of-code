import System.Environment   
import System.IO()
import qualified Data.Map as M
import qualified Data.Set as S

numGraph :: M.Map Char [Char]
numGraph = M.fromList [('A', ['0', '3']), ('0', ['A', '2']), ('1', ['2', '4']), ('2', ['0', '1', '3', '5']), ('3', ['A', '2', '6']),
                       ('4', ['1', '5', '7']), ('5', ['2', '4', '6', '8']), ('6', ['3', '5', '9']), ('7', ['4', '8']),
                       ('8', ['5', '7', '9']), ('9', ['6', '8'])]

dirpadGraph :: M.Map Char [Char]
dirpadGraph = M.fromList [('<', ['v']), ('v', ['<', '^', '>']), ('^', ['v', 'A']), ('>', ['v', 'A']), ('A', ['^', '>'])]

readLines :: FilePath -> IO[String]
readLines f = lines <$> readFile f

bfs :: M.Map Char [Char] -> [(Int, Char)] -> M.Map Char Int -> S.Set Char -> M.Map Char Int
bfs _ [] dists _ = dists
bfs graph ((cost, c):xs) dists seen
  | c `elem` seen = bfs graph xs dists seen
  | otherwise = bfs graph (xs ++ neighs) updatedDists (S.insert c seen)
  where neighs = [(cost + 1, child) | child <- graph M.! c]
        updatedDists = M.insert c cost dists

shortestPaths :: M.Map Char [Char] -> Char -> Char -> [Char] -> S.Set Char -> [[Char]]
shortestPaths graph c c' nodes seen
  | c == c' = [[]]
  | otherwise = [x:ℓ | x <- graph M.! c, x `elem` nodes, x `notElem` seen, ℓ <- next x]
  where next x = shortestPaths graph x c' nodes (S.insert c seen)

getAllPaths :: M.Map Char [Char] -> [Char] -> M.Map (Char, Char) [[Char]]
getAllPaths _ [] = M.empty
getAllPaths graph (c:cs) = foldr (\c' acc -> M.insert (c, c') (paths c') acc) (getAllPaths graph cs) (M.keys graph)
  where dists = bfs graph [(0, c)] M.empty S.empty
        paths c' = [c:ℓ | ℓ <- shortestPaths graph c c' nodes S.empty]
          where nodes = [x | x <- M.keys graph, dists M.! x + dists' M.! x == dists M.! c']
                dists' = bfs graph [(0, c')] M.empty S.empty

allPaths :: M.Map Char [Char] -> M.Map (Char, Char) [[Char]]
allPaths graph = getAllPaths graph $ M.keys graph

getShortestPathOfSeq :: Char -> String -> M.Map (Char, Char) [[Char]] -> [[Char]]
getShortestPathOfSeq _ [] _ = [""]
getShortestPathOfSeq c (x:xs) paths = [p ++ ('A':p') | p <- paths M.! (c, x), p' <- next]
  where next = getShortestPathOfSeq x xs paths

candidates :: Bool -> String -> [(Char, Char)]
candidates _ [] = []
candidates status [x]
  | status = [(x, 'A')]
  | otherwise = []
candidates status (x:y:xs) = (x,y):candidates status (y:xs)

addA :: [[Char]] -> [[Char]]
addA = map ('A':)

-- dp[s][e][L] = min{ sum(dp[x][y][L+1] | (x, y) : chars that follow each other in path) for path in paths from s to e }
-- dp[s][e][Lmax] = min { length path from s to e }
dp' :: M.Map (Char, Char, Int) Int -> M.Map (Char, Char) [[Char]] -> Int -> Char -> Char -> Int -> M.Map (Char, Char, Int) Int
dp' memo paths ℓmax s e ℓ
  | (s, e, ℓ) `M.member` memo = memo
  | ℓ == ℓmax = M.insert (s, e, ℓ) (minimum (map length (paths M.! (s, e))) + 1) memo
  | otherwise = M.insert (s, e, ℓ) (minimum [ sum [memo' M.! (x, y, ℓ + 1) | (x, y) <- candidates True path] | path <- addA $ paths M.! (s, e) ]) memo'
  where memo' = foldr (\(x,y) acc -> dp' acc paths ℓmax x y (ℓ + 1)) memo (concat [candidates True path | path <- addA $ paths M.! (s, e)])

dp :: M.Map (Char, Char) [[Char]] -> Int -> Char -> Char -> Int
dp paths ℓmax s e = memo M.! (s, e, 0)
  where memo = dp' M.empty paths ℓmax s e 0

part1 :: [String] -> M.Map (Char, Char) [[Char]] -> M.Map (Char, Char) [[Char]] -> Int
part1 xs numpadPaths dirpadPaths = foldr (\x acc -> (read . init $ x) * shortestPath ('A':x) + acc) 0 xs
  where shortestPath z = sum [minimum [sum [dp dirpadPaths 1 x y | (x, y) <- ('A', head path):candidates True path] | path <- numpadPaths M.! (c, c')] | (c, c') <- candidates False z]

part2 :: [String] -> M.Map (Char, Char) [[Char]] -> M.Map (Char, Char) [[Char]] -> Int
part2 xs numpadPaths dirpadPaths = foldr (\x acc -> (read . init $ x) * shortestPath ('A':x) + acc) 0 xs
  where shortestPath z = sum [minimum [sum [dp dirpadPaths 24 x y | (x, y) <- ('A', head path):candidates True path] | path <- numpadPaths M.! (c, c')] | (c, c') <- candidates False z]
  
oracleDir :: (Char, Char) -> Char
oracleDir (x, y)
  | (x, y) `elem` east = '<'
  | (x, y) `elem` west = '>'
  | (x, y) `elem` south = 'v'
  | (x, y) `elem` north = '^'
  where east = [('A', '0'), ('3', '2'), ('2', '1'), ('6', '5'), ('5', '4'), ('9', '8'), ('8', '7'), ('>', 'v'), ('v', '<'), ('A', '^')]
        south = [('3', 'A'), ('6', '3'), ('9', '6'), ('2', '0'), ('5', '2'), ('8', '5'), ('4', '1'), ('7', '4'), ('A', '>'), ('^', 'v')]
        west = map swap east
        north = map swap south
        swap (a, b) = (b, a)

toDir :: String -> String
toDir [x] = ""
toDir (x:y:xs) = oracleDir (x, y) : toDir (y:xs)

run :: [String] -> IO()
run content = do
  print $ part1 content numpadPaths dirpadPaths
  print $ part2 content numpadPaths dirpadPaths
  where numpadPaths = M.map (map toDir) (allPaths numGraph)
        dirpadPaths = M.map (map toDir) (allPaths dirpadGraph)

main :: IO()
main = do
  args <- getArgs
  contents <- readLines (head args)
  run contents
