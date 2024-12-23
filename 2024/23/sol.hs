import System.Environment   
import System.IO()
import Data.List.Split
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

readLines :: FilePath -> IO[(String, String)]
readLines f = map ((\ℓ -> (ℓ!!0, ℓ!!1)) . splitOn "-") . lines <$> readFile f

buildGraph :: [(String, String)] -> M.Map String [String]
buildGraph = foldr (\(x, y) acc -> M.insertWith (++) y [x] (M.insertWith (++) x [y] acc)) M.empty

triangles :: M.Map String [String] -> S.Set (S.Set String)
triangles graph = foldr (\x acc -> S.union acc $ triangle x) S.empty (M.keys graph)
  where triangle v = S.fromList [S.fromList [v, w, z] | w <- graph M.! v, z <- graph M.! w, v `elem` graph M.! z]

part1 :: M.Map String [String] -> Int
part1 connections = length . S.filter hasChiefHisto $ triangles connections
  where hasChiefHisto = any (\x -> head x == 't') . S.toList

maximalCliques :: M.Map String [String] -> S.Set String -> S.Set String -> S.Set String -> [[String]]
maximalCliques graph r p x
  | null p && null x = [S.toList r]
  | otherwise = aux (S.toList p) r x 
    where aux [] _ _ = []
          aux (v:p') r x = acc ++ aux p' r (S.insert v x)
            where acc = maximalCliques graph (S.insert v r) (S.intersection (S.fromList p') $ neighs v) (S.intersection x $ neighs v)
                  neighs v = S.fromList (graph M.! v)

part2 :: M.Map String [String] -> String
part2 connections = intercalate "," $ head $ filter ((maxLength==) . length) cliques
  where cliques = maximalCliques connections S.empty (S.fromList $ M.keys connections) S.empty
        maxLength = maximum (map length cliques)
  

main :: IO()
main = do
  args <- getArgs
  contents <- readLines (head args)
  print $ part1 (buildGraph contents)
  print $ part2 (buildGraph contents)
