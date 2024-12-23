import Lib.Graphs

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

part2 :: M.Map String [String] -> String
part2 connections = intercalate "," $ head $ filter ((maxLength==) . length) cliques
  where cliques = allMaximalCliques (M.keys connections) (connections M.!)
        maxLength = maximum (map length cliques)
  
main :: IO()
main = do
  args <- getArgs
  contents <- readLines (head args)
  print $ part1 (buildGraph contents)
  print $ part2 (buildGraph contents)
