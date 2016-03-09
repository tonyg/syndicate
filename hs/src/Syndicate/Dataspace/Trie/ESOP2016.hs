module Syndicate.Dataspace.Trie.ESOP2016 where
-- ESOP 2016 implementation of dataspace tries.
-- Includes bug fixes wrt the paper.

import Prelude hiding (null, seq)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Sigma = Open
           | Close
           | Wild
           | Ch Char
             deriving (Eq, Ord, Show)

data Trie a = Ok a
            | Tl (Trie a)
            | Br (Map.Map Sigma (Trie a))
              deriving (Eq, Show)

empty = Br Map.empty

null (Br h) = Map.null h
null _ = False

tl r = if null r then empty else Tl r

untl (Tl r) = r
untl _ = empty

route [] (Ok v) f = v
route [] _ f = f
route (_ : _) (Ok v) f = f
route (x : s) (Br h) f = if Map.null h
                         then f
                         else route s (get h x) f
route (Close : s) (Tl r) f = route s r f
route (Open : s) (Tl r) f = route s (tl (tl r)) f
route (x : s) (Tl r) f = route s (tl r) f

get h x = case Map.lookup x h of
            Just r -> r
            Nothing -> case x of
                         Open -> tl (get h Wild)
                         Close -> untl (get h Wild)
                         Wild -> empty
                         x -> get h Wild

combine r1 r2 f leftEmpty rightEmpty = g r1 r2
    where g (Tl r1) (Tl r2) = tl (g r1 r2)
          g (Tl r1) r2 = g (expand r1) r2
          g r1 (Tl r2) = g r1 (expand r2)
          g (Ok v) r2 = f (Ok v) r2
          g r1 (Ok v) = f r1 (Ok v)
          g r1 r2 | null r1 = dedup $ leftEmpty r2
          g r1 r2 | null r2 = dedup $ rightEmpty r1
          g (Br h1) (Br h2) = dedup $ Br (foldKeys g h1 h2)

foldKeys g h1 h2 = Set.foldr f Map.empty keys
    where f x acc = Map.insert x (g (get h1 x) (get h2 x)) acc
          keys = Set.union (Map.keysSet h1) (Map.keysSet h2)

expand r = Br (Map.fromList [(Wild, tl r), (Close, r)])

dedup (Br h) = Br (Map.filterWithKey (distinct h) h)

distinct h Wild r = not (null r)
distinct h Open (Tl r) = r /= get h Wild
distinct h Open r = not (null r)
distinct h Close r = r /= untl (get h Wild)
distinct h x r = r /= get h Wild

---------------------------------------------------------------------------

union r1 r2 = combine r1 r2 unionCombine id id
unionCombine (Ok vs) (Ok ws) = Ok (Set.union vs ws)
unionCombine r1 r2 | null r1 = r2
unionCombine r1 r2 | null r2 = r1

unions rs = foldr union empty rs
