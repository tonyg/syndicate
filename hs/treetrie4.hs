module TreeTrie where

-- import Debug.Trace
import Prelude hiding (null, seq)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.HUnit

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

get w h x = Map.findWithDefault w x h

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
          f x acc = update x (g (get w1 h1 x) (get w2 h2 x)) w acc
          keys = Set.union (Map.keysSet h1) (Map.keysSet h2)

collapse (Br (os, Mt, h)) | Map.null os && Map.null h = empty
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

seqs _ r | null r = r
seqs [] r = r
seqs ('<' : n : s) r = Br (Map.singleton (read (n : []) :: Integer) (seqs s r), Mt, Map.empty)
seqs ('*' : s) r = Br (Map.empty, seqs s r, Map.empty)
seqs (x : s) r = Br (Map.empty, Mt, Map.singleton x (seqs s r))

main = runTestTT $
       test [
             "seqs simple" ~:
               Br (Map.singleton 0 (ok [1]), Mt, Map.empty) ~=? seqs "<0" (ok [1]),
             "union simple1" ~:
               Br (Map.empty, Mt,
                   Map.fromList [('a', ok [1]),
                                 ('b', ok [2])]) ~=?
               union (seqs "a" (ok [1])) (seqs "b" (ok [2])),
             "union simple2" ~:
               Br (Map.empty, Mt,
                   Map.fromList [('a', ok [1,2]),
                                 ('b', ok [2])]) ~=?
               unions [seqs "a" (ok [1]),
                       seqs "b" (ok [2]),
                       seqs "a" (ok [2])],
             "union idem" ~:
               (seqs "abc" (ok [1])) ~=?
               union (seqs "abc" (ok [1])) (seqs "abc" (ok [1])),
             "union wild" ~:
               Br (Map.singleton 1 (Br (Map.empty,
                                        ok [1],
                                        Map.singleton 'a' (ok [1,2]))),
                   ok [1],
                   Map.empty) ~=?
               union (seqs "*" (ok [1])) (seqs "<1a" (ok [2])),
             "route union wild1" ~: Set.fromList [1,2] ~=?
                                 route "<1a" (union
                                              (seqs "*" (ok [1]))
                                              (seqs "<1a" (ok [2]))) Set.empty,
             "route union wild2" ~: Set.fromList [1] ~=?
                                 route "<1b" (union
                                              (seqs "*" (ok [1]))
                                              (seqs "<1a" (ok [2]))) Set.empty,
             "route union wild3" ~: Set.fromList [1] ~=?
                                 route "<0" (union
                                             (seqs "*" (ok [1]))
                                             (seqs "<1a" (ok [2]))) Set.empty,
             "route union wild4" ~: Set.fromList [1] ~=?
                                 route "<2aa" (union
                                               (seqs "*" (ok [1]))
                                               (seqs "<1a" (ok [2]))) Set.empty,
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
               Br (Map.empty,
                   ok [1],
                   Map.fromList [('a', Mt)]) ~=?
               difference (seqs "*" (ok [1])) (seqs "a" (ok [1])),
             "difference wild 2" ~:
               Br (Map.singleton 1 (Br (Map.empty,
                                        ok [1],
                                        Map.singleton 'a' Mt)),
                   ok [1],
                   Map.empty) ~=?
               difference (seqs "*" (ok [1])) (seqs "<1a" (ok [1])),
             "difference wild 3" ~:
               Br (Map.singleton 0 Mt,
                   ok [1],
                   Map.empty) ~=?
               difference (seqs "*" (ok [1])) (seqs "<0" (ok [1])),
             "union after difference" ~:
               seqs "*" (ok [1]) ~=?
               union (difference (seqs "*" (ok [1])) (seqs "a" (ok [1]))) (seqs "a" (ok [1])),
             "union after difference 2" ~:
               Br (Map.empty,
                   ok [1],
                   Map.fromList [('a', ok [2])]) ~=?
               union (difference (seqs "*" (ok [1])) (seqs "a" (ok [1]))) (seqs "a" (ok [2])),
             "intersection no overlap opens" ~:
               empty ~=?
               intersection (seqs "<2aa" (ok [1])) (seqs "<1b" (ok [2])),
             "intersection no overlap opens 2" ~:
               Br (Map.empty, Mt, Map.singleton 'x' (ok [1,2])) ~=?
               (intersection
                (union (seqs "x" (ok [1])) (seqs "<2aa" (ok [1])))
                (union (seqs "x" (ok [2])) (seqs "<1b" (ok [2])))),
             "intersection no overlap opens 3" ~:
               Br (Map.fromList [(1,Br (Map.empty,
                                        ok [3,4],
                                        Map.fromList [('b', ok [2,3,4])])),
                                 (2,Br (Map.empty,
                                        Br (Map.empty, ok [3,4], Map.empty),
                                        Map.fromList [('a',Br (Map.empty,
                                                               ok [3,4],
                                                               Map.fromList [('a',
                                                                              ok [1,3,4])]))]))],
                   ok [3,4],
                   Map.empty) ~=?
               (intersection
                (union (seqs "*" (ok [3])) (seqs "<2aa" (ok [1])))
                (union (seqs "*" (ok [4])) (seqs "<1b" (ok [2]))))
            ]
