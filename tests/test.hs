
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Consensus.Types
import TestStore

------------------------------------------------------------

main = defaultMain tests

------------------------------------------------------------

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "Query empty" $
      testQuery 0 empty
        @?= Nothing
  , testCase "Store seven" $
      testQuery 0 (testStore 0 [7] (Term 1) empty)
        @?= Just (7, Term 1)
  , testCase "Store nine" $
      testQuery 2 (testStore 1 [8,9] (Term 1) (testStore 0 [7] (Term 1) empty))
        @?= Just (9, Term 1)
  , testCase "Truncate" $
      testQuery 2 (testTruncate 1 $ testStore 1 [8,9] (Term 1) (testStore 0 [7] (Term 1) empty))
        @?= Nothing

  , testCase "run1" $
      testState (query' 0) 0
        @?= Nothing
  ]
