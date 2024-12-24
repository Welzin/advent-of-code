import Lib.Graphs

import System.Environment   
import System.IO()
import Data.List.Split
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Bits

readLines :: FilePath -> IO[[String]]
readLines f = map lines . splitOn "\n\n" <$> readFile f

initMem :: [String] -> M.Map String Int
initMem = foldr (\x acc -> M.insert (s x) (v x) acc) M.empty
  where s x = head $ splitOn ": " x
        v x = read (head . tail $ splitOn ": " x)

compute :: M.Map String Int -> [String] -> [String] -> [String] -> M.Map String Int
compute mem [] [] _ = mem
compute mem [] ℓ ℓ'
  | ℓ == ℓ' = M.map (\_ -> 1) mem
  | otherwise = compute mem ℓ [] ℓ
compute mem (x:xs) ℓ ℓ'
  | s `M.member` mem && e `M.member` mem = compute (M.insert t v mem) xs ℓ ℓ'
  | otherwise = compute mem xs (ℓ ++ [x]) ℓ'
  where t = head . tail $ splitOn " -> " x
        y = words x
        s = y !! 0
        op = y !! 1
        e = y !! 2
        v = case op of
              "AND" -> apply (*)
              "XOR" -> apply xor
              "OR" -> apply max
          where apply f = f (mem M.! s) (mem M.! e)

part1 :: M.Map String Int -> [String] -> Integer
part1 mem xs = foldr (\x acc -> acc + v x * 2 ^ z x) 0 $ filter (('z'==) . head) (M.keys updatedMem)
  where z = read . tail
        v x = toInteger (updatedMem M.! x)
        updatedMem = compute mem xs [] []

run :: M.Map String Int -> [String] -> IO()
run mem xs = do
  print $ part1 mem xs
  print $ intercalate "," (S.toList $ S.fromList ["wss", "wrm", "z08", "z22", "z29", "thm", "hwq", "gbs"])

main :: IO()
main = do
  args <- getArgs
  contents <- readLines (head args)
  run (initMem (head contents)) (head . tail $ contents)
