module Lib.Memoization where

import Data.MemoTrie as M
import Data.Function (fix)

-- A small memoization library DP problems.

type Memo f = f -> f

memo1 :: M.HasTrie a => Memo (a -> b) -> a -> b
memo1 f = fix (M.memo . f)

memo2 :: (M.HasTrie a, M.HasTrie b) => Memo (a -> b -> c) -> a -> b -> c
memo2 f = fix (M.memo2 . f)

memo3 :: (M.HasTrie a, M.HasTrie b, M.HasTrie c) => Memo (a -> b -> c -> d) -> a -> b -> c -> d
memo3 f = fix (M.memo3 . f)

memo4 :: (M.HasTrie a, M.HasTrie b, M.HasTrie c, M.HasTrie d) => Memo (a -> b -> c -> d -> e) -> a -> b -> c -> d -> e
memo4 f = fix (M.mup M.memo3 . f)
