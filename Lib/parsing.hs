{-# LANGUAGE FunctionalDependencies #-}
module Lib.Parsing where

import System.IO()
import Data.List.Split
import qualified Data.Array as A
-- A small module for easing the parsing of AoC's problems.

-- # Base parsing functions

parseStrings :: FilePath -> IO[String]
parseStrings f = lines <$> readFile f

parseInts :: FilePath -> IO[Int]
parseInts f = map read <$> parseStrings f

parseSplit :: String -> FilePath -> IO[String]
parseSplit c f = splitOn c <$> readFile f

parseSplitInts :: String -> FilePath -> IO[Int]
parseSplitInts c f = map read <$> parseSplit c f

-- # Grids (i.e., n-dimensional fast-access data structures)

-- The free variables [i] and [a] are not really free, they are uniquely determined by [g].
class Grid g i a | g -> a, g -> i where
  (!) :: g -> i -> a
  (//) :: g -> [(i, a)] -> g

type Grid1D a = A.Array Int a
type Grid2D a = A.Array (Int, Int) a
type Grid3D a = A.Array (Int, Int, Int) a

instance Grid (Grid1D a) Int a where
  (!) = (A.!)
  (//) = (A.//)

instance Grid (Grid2D a) (Int, Int) a where
  (!) = (A.!)
  (//) = (A.//)

instance Grid (Grid3D a) (Int, Int, Int) a where
  (!) = (A.!)
  (//) = (A.//)

parseGrid1D :: FilePath -> (FilePath -> IO[a]) -> IO (Grid1D a)
parseGrid1D f p = do
  grid <- p f
  return $ A.listArray (0, length grid - 1) grid

parseGrid1DInts :: FilePath -> IO (Grid1D Int)
parseGrid1DInts f = parseGrid1D f parseInts

parseGrid1DIntsSplit :: String -> FilePath -> IO (Grid1D Int)
parseGrid1DIntsSplit c f = parseGrid1D f (parseSplitInts c)
