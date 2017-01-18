{-# LANGUAGE FlexibleInstances #-}
module Syndicate.Dataspace.Trie.Prefix where
-- Alternate representation, where Open has an explicit *arity*
-- attached to it, and matching close-parens are implicitly tracked.
-- Where ESOP2016-style implementations have "<xyz>", this style has
-- "<3xyz".

import Prelude hiding (null, seq)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Trie a = Mt
            | Ok a
            | Br (Map.Map Integer (Trie a), Trie a, Map.Map Char (Trie a)) -- Opens, Wild, rest
              deriving (Eq, Show)

empty = Mt

null Mt = True
null _ = False

makeTail _ r | null r = r
makeTail 0 r = r
makeTail n r = Br (Map.empty, makeTail (n - 1) r, Map.empty)

stripTail _ r | null r = Just r
stripTail 0 r = Just r
stripTail n (Br (os, r, h)) | Map.null os && Map.null h = stripTail (n - 1) r
stripTail _ _ = Nothing

route _ Mt f = f
route [] (Ok v) f = v
route [] _ f = f
route (_ : _) (Ok v) f = f
route ('<' : nc : s) (Br (os, w, _)) f =
    let n = (read (nc : []) :: Integer) in
    case Map.lookup n os of
      Just r -> route s r f
      Nothing -> route s (makeTail n w) f
route (x : s) (Br (_, w, h)) f = route s (Map.findWithDefault w x h) f

combine f leftEmpty rightEmpty r1 r2 = g r1 r2
    where g (Ok v) r2 = f (Ok v) r2
          g r1 (Ok v) = f r1 (Ok v)
          g r1 r2 | null r1 = collapse $ leftEmpty r2
          g r1 r2 | null r2 = collapse $ rightEmpty r1
          g r1 r2 = collapse $ foldKeys g r1 r2

foldKeys g (Br (os1, w1, h1)) (Br (os2, w2, h2)) =
    Br (Set.foldr fo Map.empty sizes, w, Set.foldr f Map.empty keys)
    where sizes = Set.union (Map.keysSet os1) (Map.keysSet os2)
          w = g w1 w2
          fo size acc = let o1 = Map.findWithDefault (makeTail size w1) size os1 in
                        let o2 = Map.findWithDefault (makeTail size w2) size os2 in
                        let o = g o1 o2 in
                        if stripTail size o == Just w then acc else Map.insert size o acc
          f x acc = update x (g (Map.findWithDefault w1 x h1) (Map.findWithDefault w2 x h2)) w acc
          keys = Set.union (Map.keysSet h1) (Map.keysSet h2)

collapse (Br (os, Mt, h)) | Map.null os && Map.null h = empty
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
