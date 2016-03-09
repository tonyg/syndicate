module TreeTrie where

-- import Debug.Trace
import Prelude hiding (null, seq)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.HUnit

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

union = combine unionCombine id id
unionCombine (Ok vs) (Ok ws) = Ok (Set.union vs ws)
unionCombine r1 r2 | null r1 = r2
unionCombine r1 r2 | null r2 = r1

unions rs = foldr union empty rs

intersection = combine intersectionCombine (const empty) (const empty)
intersectionCombine (Ok vs) (Ok ws) = Ok (Set.union vs ws)
intersectionCombine r1 r2 | null r1 = empty
intersectionCombine r1 r2 | null r2 = empty

difference = combine differenceCombine (const empty) id
differenceCombine (Ok vs) (Ok ws) = let xs = Set.difference vs ws in
                                    if Set.null xs then empty else (Ok xs)
differenceCombine r1 r2 | null r1 = empty
differenceCombine r1 r2 | null r2 = r1

---------------------------------------------------------------------------

ok vs = Ok (Set.fromList vs)

seq _ r | null r = r
seq '<' r = Br (r, Mt, Mt, Map.empty)
seq '>' r = Br (Mt, r, Mt, Map.empty)
seq '*' r = Br (tl r, untl r, r, Map.empty)
seq x r = Br (Mt, Mt, Mt, Map.singleton x r)

seqs s r = foldr seq r s

main = runTestTT $
       test [
             "seqs simple" ~:
               Br (Br (Mt, ok [1], Mt, Map.empty), Mt, Mt, Map.empty) ~=? seqs "<>" (ok [1]),
             "union simple1" ~:
               Br (Mt, Mt, Mt,
                   Map.fromList [('a', ok [1]),
                                 ('b', ok [2])]) ~=?
               union (seqs "a" (ok [1])) (seqs "b" (ok [2])),
             "union simple2" ~:
               Br (Mt, Mt, Mt,
                   Map.fromList [('a', ok [1,2]),
                                 ('b', ok [2])]) ~=?
               unions [seqs "a" (ok [1]),
                       seqs "b" (ok [2]),
                       seqs "a" (ok [2])],
             "union idem" ~:
               (seqs "abc" (ok [1])) ~=?
               union (seqs "abc" (ok [1])) (seqs "abc" (ok [1])),
             "union wild" ~:
               Br (Br (Mt,
                       ok [1],
                       Tl (ok [1]),
                       Map.fromList [('a', Br (Mt,
                                               ok [1,2],
                                               Tl (ok [1]),
                                               Map.empty))]),
                   Mt,
                   ok [1],
                   Map.empty) ~=?
               union (seqs "*" (ok [1])) (seqs "<a>" (ok [2])),
             "route union wild1" ~: Set.fromList [1,2] ~=?
                                 route "<a>" (union
                                              (seqs "*" (ok [1]))
                                              (seqs "<a>" (ok [2]))) Set.empty,
             "route union wild2" ~: Set.fromList [1] ~=?
                                 route "<b>" (union
                                              (seqs "*" (ok [1]))
                                              (seqs "<a>" (ok [2]))) Set.empty,
             "route union wild3" ~: Set.fromList [1] ~=?
                                 route "<>" (union
                                             (seqs "*" (ok [1]))
                                             (seqs "<a>" (ok [2]))) Set.empty,
             "route union wild4" ~: Set.fromList [1] ~=?
                                 route "<aa>" (union
                                               (seqs "*" (ok [1]))
                                               (seqs "<a>" (ok [2]))) Set.empty,
             "intersection simple1" ~:
               seqs "a" (ok [1,2]) ~=? intersection (seqs "a" (ok [1])) (seqs "a" (ok [2])),
             "intersection simple2" ~:
               empty ~=? intersection (seqs "a" (ok [1])) (seqs "b" (ok [2])),
             "intersection idem" ~:
               (seqs "abc" (ok [1])) ~=?
               intersection (seqs "abc" (ok [1])) (seqs "abc" (ok [1])),
             "difference simple1" ~:
               seqs "a" (ok [1]) ~=? difference (seqs "a" (ok [1,2])) (seqs "a" (ok [2])),
             "difference simple1a" ~:
               seqs "ab" (ok [1]) ~=? difference (seqs "ab" (ok [1,2])) (seqs "ab" (ok [2])),
             "difference simple2" ~:
               empty ~=? difference (seqs "a" (ok [1])) (seqs "a" (ok [1])),
             "difference wild" ~:
               Br (Tl (ok [1]),
                   Mt,
                   ok [1],
                   Map.fromList [('a', Mt)]) ~=?
               difference (seqs "*" (ok [1])) (seqs "a" (ok [1])),
             "union after difference" ~:
               seqs "*" (ok [1]) ~=?
               union (difference (seqs "*" (ok [1])) (seqs "a" (ok [1]))) (seqs "a" (ok [1])),
             "union after difference 2" ~:
               Br (Tl (ok [1]),
                   Mt,
                   ok [1],
                   Map.fromList [('a', ok [2])]) ~=?
               union (difference (seqs "*" (ok [1])) (seqs "a" (ok [1]))) (seqs "a" (ok [2]))
            ]
