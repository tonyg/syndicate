{-# LANGUAGE FlexibleInstances #-}
module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Syndicate.Dataspace.Trie.Tests.ESOP2016 as ESOP2016
import Syndicate.Dataspace.Trie.Tests.ESOP2016v2 as ESOP2016v2
import Syndicate.Dataspace.Trie.Tests.ESOP2016v3 as ESOP2016v3
import Syndicate.Dataspace.Trie.Tests.Prefix as Prefix

testOpts = (mempty :: TestOptions)
  { topt_maximum_generated_tests = Just 1000
  , topt_maximum_unsuitable_generated_tests = Just 10000
  }
runnerOpts = (mempty :: RunnerOptions) { ropt_test_options = Just testOpts }
runTests tests = defaultMainWithOpts tests runnerOpts

main = runTests
  [ testGroup "ESOP2016" $ hUnitTestToTests ESOP2016.hUnitSuite
  , testGroup "ESOP2016v2" $ hUnitTestToTests ESOP2016v2.hUnitSuite
  , testGroup "ESOP2016v3" $ hUnitTestToTests ESOP2016v3.hUnitSuite
  , testGroup "Prefix" [ testGroup "HUnit tests" $ hUnitTestToTests Prefix.hUnitSuite
                       , testGroup "QuickCheck tests" Prefix.quickCheckSuite
                       ]
  ]
