module Lib.Graphs where

import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
-- A small graph library for the problems of the advent of code.

-- # Structural utilities

-- Building a graph from a list of pairs
fromPairList :: Ord a => [(a, a)] -> a -> [a]
fromPairList xs = (graph M.!)
  where graph = foldr (\(x, y) acc -> M.insertWith (++) x [y] acc) M.empty xs

-- Given a set of vertices and a neighbour function, returns a neighbour function that has reversed
-- all the edges of the graph.
reverseGraph :: Ord a => [a] -> (a -> [a]) -> a -> [a]
reverseGraph vertices f = (reversedGraph M.!)
  where reversedGraph = foldr (\x acc -> foldr (\y -> M.insertWith (++) y [x]) acc (f x)) (M.fromList [(x, []) | x <- vertices]) vertices

-- Given a vertex and a neighbours function, returns the set of vertices seen together with the
-- exploration tree (stored as a map of parents).
dfs :: Ord a => a -> (a -> [a]) -> (S.Set a, M.Map a a)
dfs x f = aux x S.empty $ M.singleton x x
  where aux u seen parent
          | u `elem` seen = (seen, parent)
          | otherwise = foldr (\y (s, p) -> aux y s (M.insert y u p)) (S.insert u seen, parent) (f u)

-- # Graph exploration and path reconstruction

-- Given a vertex and a neighbours function, returns the distance of each reachable vertex together
-- with the exploration tree (stored as a map of parents).
bfs :: Ord a => a -> (a -> [a]) -> (M.Map a Int, M.Map a a)
bfs x f = aux [(x, x)] (M.singleton x (-1)) M.empty
  where aux [] dist parent = (dist, parent)
        aux ((p, y) : xs) dist parent
          | y `elem` (M.keys parent) = aux xs dist parent
          | otherwise = aux next (M.insert y ((dist M.! p) + 1) dist) (M.insert y p parent)
          where next = xs ++ [(y, z) | z <- f y]

-- Given a vertex, a neighbours function and a cost function, returns the distance of each reachable
-- vertex together with the exploration tree (stored as a map of parents).
-- The cost function should define a pseudo-metric: c x x = 0, c x y = c y x and triangular
-- inequality.
dijkstra :: Ord a => a -> (a -> [a]) -> (a -> a -> Int) -> (M.Map a Int, M.Map a a)
dijkstra x f c = aux (S.singleton (0, x)) M.empty M.empty
  where aux queue dist parent =
          case S.minView queue of
            Nothing -> (dist, parent)
            Just ((cost, u), uqueue) -> aux (S.union uqueue neighs) (M.insert u cost dist) p
              where neighs = S.fromList [(dist M.! u + c u y, y) | y <- f u]
                    p = foldr (\y acc -> M.insert y u acc) parent (f u)

-- Returns a path from a map of parents (obtained by the dfs, bfs or dijkstra algorithms of this
-- file), given the last vertex of the would-be path.
path :: (Eq a, Ord a) => a -> M.Map a a -> [a]
path x parent = aux x []
  where aux y acc
          | parent M.! y == y = y : acc
          | otherwise = aux (parent M.! y) (y : acc)

-- Given two vertices s and t, a neighbour function, a cost function and the set of all vertices,
-- returns all the nodes that are on a shortest path between s and t.
allNodesShortestPath :: Ord a => a -> a -> (a -> [a]) -> (a -> a -> Int) -> [a] -> [a]
allNodesShortestPath s t f c vertices = filter rightWeight vertices
  where (ds, _) = dijkstra s f c
        (dt, _) = dijkstra t f c
        rightWeight u = ds M.! u + dt M.! u == ds M.! t

-- Internal function. Do not call as is.
allShortestPaths' :: Ord a => a -> a -> (a -> [a]) -> [a] -> [a] -> S.Set a -> [[a]]
allShortestPaths' s t f vertices nodes seen = [s : ℓ | ℓ <- aux s seen]
  where aux x seen
          | x == t = [[]]
          | otherwise = [y : ℓ | y <- f x, y `elem` nodes, y `notElem` seen, ℓ <- next y]
          where next y = aux y (S.insert x seen)

-- Given two vertices s and t, a neighbour function, a cost function and the set of all vertices,
-- returns all the shortest paths from s to t.
allShortestPaths :: Ord a => a -> a -> (a -> [a]) -> (a -> a -> Int) -> [a] -> [[a]]
allShortestPaths s t f c vertices = allShortestPaths' s t f vertices nodes S.empty
  where nodes = allNodesShortestPath s t f c vertices

-- Given two vertices s and t, a neighbour function and the set of all vertices, returns all the
-- nodes that are on a shortest path between s and t.
-- U stands for unweighted.
allNodesShortestPathU :: Ord a => a -> a -> (a -> [a]) -> [a] -> [a]
allNodesShortestPathU s t f vertices = filter rightWeight vertices
  where (ds, _) = bfs s f
        (dt, _) = bfs t f
        rightWeight u = ds M.! u + dt M.! u == ds M.! t

-- Given two vertices s and t, a neighbour function and the set of all vertices, returns all the
-- shortest paths from s to t (unweighted version).
allShortestPathsU :: Ord a => a -> a -> (a -> [a]) -> [a] -> [[a]]
allShortestPathsU s t f vertices = allShortestPaths' s t f vertices nodes S.empty
  where nodes = allNodesShortestPathU s t f vertices

-- Given a set of vertices, and a neighbour function, returns all the strongly connected components
-- of a graph.  The strongly connected components are returned as a map of representents r & list of
-- vertices in the connected component represented by r. This is implemented using Kosaraju's
-- algorithm (https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm).
scc :: Ord a => [a] -> (a -> [a]) -> M.Map a [a]
scc vertices f = foldr (\x acc -> assign x x acc) M.empty ℓ
  where (_, ℓ) = foldr add (S.empty, []) vertices
        aux x seen
          | x `elem` seen = (seen, [])
          | otherwise = (newSeen, x : newAcc)
          where (newSeen, newAcc) = foldr add (S.insert x seen, []) (f x)
        add y (seen, acc) = (newSeen, newAcc ++ acc)
          where (newSeen, newAcc) = aux y seen
        assign x root assigned
          | any (x `elem`) (M.elems assigned) = assigned
          | otherwise = foldr (\y acc -> assign y root acc) (M.insertWith (++) root [x] assigned) (f x) 

-- # Others operations

-- Given a set of vertices and a neighbour function, get all the maximal cliques in a graph using
-- the Bron-Kerbosch algorithm (https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm).
allMaximalCliques :: Ord a => [a] -> (a -> [a]) -> [[a]]
allMaximalCliques vertices f = bronKerbosch S.empty (S.fromList vertices) S.empty
  where bronKerbosch r p x
          | null p && null x = [S.toList r]
          | otherwise = aux (S.toList p) r x 
          where aux [] _ _ = []
                aux (v:p') r x = acc ++ aux p' r (S.insert v x)
                  where acc = bronKerbosch (S.insert v r) (S.fromList $ intersect p' $ f v) (S.intersection x $ (S.fromList $ f v))
