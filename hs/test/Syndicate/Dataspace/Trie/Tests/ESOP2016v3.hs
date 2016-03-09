module Syndicate.Dataspace.Trie.Tests.ESOP2016v3 where
-- Explicitly separate Open/Close/Wild from other edges in Br nodes.
-- This gives an elegant presentation.

import Prelude hiding (null, seq)
import Syndicate.Dataspace.Trie.ESOP2016v3
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.HUnit

ok vs = Ok (Set.fromList vs)

seq _ r | null r = r
seq '<' r = Br (r, Mt, Mt, Map.empty)
seq '>' r = Br (Mt, r, Mt, Map.empty)
seq '*' r = Br (tl r, untl r, r, Map.empty)
seq x r = Br (Mt, Mt, Mt, Map.singleton x r)

seqs s r = foldr seq r s

hUnitSuite = test
  [ "seqs simple" ~:
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
