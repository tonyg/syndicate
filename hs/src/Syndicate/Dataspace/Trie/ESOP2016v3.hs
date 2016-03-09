module Syndicate.Dataspace.Trie.ESOP2016v3 where
-- Explicitly separate Open/Close/Wild from other edges in Br nodes.
-- This gives an elegant presentation.

import Prelude hiding (null, seq)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Trie a = Mt
            | Ok a
            | Tl (Trie a)
            | Br (Trie a, Trie a, Trie a, Map.Map Char (Trie a)) -- Open, Close, Wild, rest
              deriving (Eq, Show)

empty = Mt

null Mt = True
null _ = False

tl r = if null r then empty else Tl r

untl (Tl r) = r
untl _ = empty

route _ Mt f = f
route [] (Ok v) f = v
route [] _ f = f
route (_ : _) (Ok v) f = f
route ('<' : s) (Br (r, _, _, _)) f = route s r f
route ('>' : s) (Br (_, r, _, _)) f = route s r f
route (x : s)   (Br (_, _, w, h)) f = route s (Map.findWithDefault w x h) f
route ('<' : s) (Tl r) f = route s (tl (tl r)) f
route ('>' : s) (Tl r) f = route s r f
route (x : s) (Tl r) f = route s (tl r) f

get w h x = Map.findWithDefault w x h

combine f leftEmpty rightEmpty r1 r2 = g r1 r2
    where g (Tl r1) (Tl r2) = tl (g r1 r2)
          g (Tl r1) r2 = g (expand r1) r2
          g r1 (Tl r2) = g r1 (expand r2)
          g (Ok v) r2 = f (Ok v) r2
          g r1 (Ok v) = f r1 (Ok v)
          g r1 r2 | null r1 = collapse $ leftEmpty r2
          g r1 r2 | null r2 = collapse $ rightEmpty r1
          g r1 r2 = collapse $ foldKeys g r1 r2

foldKeys g (Br (o1, c1, w1, h1)) (Br (o2, c2, w2, h2)) =
    Br (g o1 o2, g c1 c2, w, Set.foldr f Map.empty keys)
    where w = g w1 w2
          f x acc = update x (g (get w1 h1 x) (get w2 h2 x)) w acc
          keys = Set.union (Map.keysSet h1) (Map.keysSet h2)

expand r = Br (Mt, r, tl r, Map.empty)

collapse (Br (Mt, k, Tl k', h)) | Map.null h && k == k' = tl k
collapse (Br (Mt, Mt, Tl k, h)) | Map.null h = tl k
collapse (Br (Mt, Mt, Mt, h)) | Map.null h = empty
collapse r = r

update x k w h = if k == w then Map.delete x h else Map.insert x k h

---------------------------------------------------------------------------

union :: Ord t => Trie (Set.Set t) -> Trie (Set.Set t) -> Trie (Set.Set t)
union = combine unionCombine id id
unionCombine (Ok vs) (Ok ws) = Ok (Set.union vs ws)
unionCombine r1 r2 | null r1 = r2
unionCombine r1 r2 | null r2 = r1

unions rs = foldr union empty rs

intersection :: Ord t => Trie (Set.Set t) -> Trie (Set.Set t) -> Trie (Set.Set t)
intersection = combine intersectionCombine (const empty) (const empty)
intersectionCombine (Ok vs) (Ok ws) = Ok (Set.union vs ws)
intersectionCombine r1 r2 | null r1 = empty
intersectionCombine r1 r2 | null r2 = empty

difference :: Ord t => Trie (Set.Set t) -> Trie (Set.Set t) -> Trie (Set.Set t)
difference = combine differenceCombine (const empty) id
differenceCombine (Ok vs) (Ok ws) = let xs = Set.difference vs ws in
                                    if Set.null xs then empty else (Ok xs)
differenceCombine r1 r2 | null r1 = empty
differenceCombine r1 r2 | null r2 = r1
