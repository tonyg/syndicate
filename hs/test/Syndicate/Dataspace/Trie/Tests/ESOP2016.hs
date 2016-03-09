module Syndicate.Dataspace.Trie.Tests.ESOP2016 where

import Prelude hiding (null, seq)
import Syndicate.Dataspace.Trie.ESOP2016
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.HUnit

ok vs = Ok (Set.fromList vs)
seq x r = if null r then r else Br (Map.singleton x r)

seqCh '<' = Open
seqCh '>' = Close
seqCh '*' = Wild
seqCh x = Ch x

seqs s r = foldr (\ x r -> seq (seqCh x) r) r s

hUnitSuite = test
  [ "seqs simple" ~: seq Open (seq Close (Ok (Set.singleton 1))) ~=? seqs "<>" (ok [1]),
    "union simple1" ~: Br (Map.fromList [(Ch 'a', ok [1]),
                                         (Ch 'b', ok [2])]) ~=?
                    union (seqs "a" (ok [1])) (seqs "b" (ok [2])),
    "union simple2" ~: Br (Map.fromList [(Ch 'a', ok [1,2]),
                                         (Ch 'b', ok [2])]) ~=?
                    unions [seqs "a" (ok [1]),
                            seqs "b" (ok [2]),
                            seqs "a" (ok [2])],
    "union idem" ~: (seqs "abc" (ok [1])) ~=?
                 union (seqs "abc" (ok [1])) (seqs "abc" (ok [1])),
    "union wild" ~:
                 -- This is noisier than it needs to be.
                 Br (Map.fromList [(Open,Br (Map.fromList [(Close, ok [1]),
                                                           (Wild,Br (Map.fromList [(Wild,Tl (ok [1]))])),
                                                           (Ch 'a',Br (Map.fromList [(Close, ok [1,2]),
                                                                                     (Wild,Br (Map.fromList [(Wild,Tl (ok [1]))]))]))])),
                                   (Wild, ok [1])])
                 ~=? union (seqs "*" (ok [1])) (seqs "<a>" (ok [2])),
    "route union wild1" ~: Set.fromList [1,2] ~=?
                        route [Open, Ch 'a', Close] (union
                                                     (seqs "*" (ok [1]))
                                                     (seqs "<a>" (ok [2]))) Set.empty,
    "route union wild2" ~: Set.fromList [1] ~=?
                        route [Open, Ch 'b', Close] (union
                                                     (seqs "*" (ok [1]))
                                                     (seqs "<a>" (ok [2]))) Set.empty,
    "route union wild3" ~: Set.fromList [1] ~=?
                        route [Open, Close] (union
                                             (seqs "*" (ok [1]))
                                             (seqs "<a>" (ok [2]))) Set.empty,
    "route union wild4" ~: Set.fromList [1] ~=?
                        route [Open, Ch 'a', Ch 'a', Close] (union
                                                             (seqs "*" (ok [1]))
                                                             (seqs "<a>" (ok [2]))) Set.empty
  ]
