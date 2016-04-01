{-# LANGUAGE FlexibleInstances #-}
module Syndicate.Dataspace.Trie.Tests.Prefix where
-- Alternate representation, where Open has an explicit *arity*
-- attached to it, and matching close-parens are implicitly tracked.
-- Where ESOP2016-style implementations have "<xyz>", this style has
-- "<3xyz".

import Prelude hiding (null)
import Syndicate.Dataspace.Trie.Prefix
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Test.HUnit
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Control.Monad

ok vs = Ok (Set.fromList vs)

seqs _ r | null r = r
seqs [] r = r
seqs ('<' : n : s) r = Br (Map.singleton (read (n : []) :: Integer) (seqs s r), Mt, Map.empty)
seqs ('*' : s) r = Br (Map.empty, seqs s r, Map.empty)
seqs (x : s) r = Br (Map.empty, Mt, Map.singleton x (seqs s r))

hUnitSuite = test
  [ "seqs simple" ~:
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

---------------------------------------------------------------------------

newtype Pattern = Pattern { getPattern :: String } deriving (Eq, Ord, Show)
newtype Message = Message { getMessage :: String } deriving (Eq, Ord, Show)

instance Arbitrary Pattern where
    arbitrary = liftM Pattern $ sized $ trieNoLargerThan
        where leaf = oneof $ [return "x",
                              return "y",
                              return "z",
                              return "*"]
              trieNoLargerThan leafLimit =
                  if leafLimit >= 1
                  then frequency [(2, leaf), (3, node leafLimit)]
                  else leaf
              node leafLimit =
                  do degree <- choose (0, min 4 leafLimit)
                     kids <- genChildren leafLimit degree
                     return $ "<" ++ show degree ++ concat kids
              genChildren leafLimit 0 = return []
              genChildren leafLimit degree =
                  do childLimit <- choose (1, leafLimit - (degree - 1))
                     child <- trieNoLargerThan childLimit
                     rest <- genChildren (leafLimit - childLimit) (degree - 1)
                     return (child : rest)

instance Arbitrary Message where
    arbitrary = do Pattern p <- arbitrary
                   m <- sequence $ [if c == '*'
                                    then do Message m <- scale (`div` 2) arbitrary
                                            return m
                                    else return (c : [])
                                        | c <- p]
                   return $ Message $ concat m

instance Arbitrary (Set.Set Integer) where
    arbitrary = resize 5 $ sized set
        where set 0 = return Set.empty
              set n = do v <- arbitrary `suchThat` (\v -> v >= 0)
                         s <- set (n - 1)
                         return $ Set.insert v s

genTrie k 0 = return Mt
genTrie k n = do Pattern p <- arbitrary
                 rest <- genTrie k (n - 1)
                 return $ union (seqs p k) rest

type TrieOfPids = Trie (Set.Set Integer)

instance Arbitrary TrieOfPids where
    -- arbitrary = do vs <- arbitrary
    --                resize 6 $ sized $ genTrie (Ok vs)
    arbitrary = resize 6 $ sized $ genTrie (ok [1])

isWild (Br (os, w, h)) = Map.null os && Map.null h
isWild _ = False

trieContains t (Message m) = not $ Set.null $ route m t Set.empty

combineBasics :: (TrieOfPids -> TrieOfPids -> TrieOfPids) ->
                 (Bool -> Bool -> Bool) ->
                 (TrieOfPids, TrieOfPids, Message) ->
                 Property
combineBasics tf bf (trie1, trie2, element) =
    not (isWild trie1) && not (isWild trie2) && (p || q1 || q2) ==> p == q
        where p = combined `trieContains` element
              q1 = trie1 `trieContains` element
              q2 = trie2 `trieContains` element
              q = bf q1 q2
              combined = tf trie1 trie2

unionBasics = combineBasics union (||)
intersectionBasics = combineBasics intersection (&&)
differenceBasics = combineBasics difference (\ x y -> x && not y)

quickCheckSuite = [ testProperty "differenceBasics" differenceBasics
                  , testProperty "intersectionBasics" intersectionBasics
                  , testProperty "unionBasics" unionBasics
                  ]
