
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import TestStore

------------------------------------------------------------

main = defaultMain tests

------------------------------------------------------------

tests :: TestTree
tests = testGroup "Tests" [unitTests]

voom :: [a] -> [a]
voom _ = []

unitTests = testGroup "Unit tests"
  [ testCase "Store nothing" $
      voom ([] :: [(Int, Maybe Int)])
        @?= []
  , testCase "Store nothing again" $
      voom [(1, Just 2), (2, Just 3)]
        @?= []
  , testCase "Store zero items" $
      voom [(1, Just 2), (7, Just 8), (2, Just 3)]
        @?= []
  , testCase "Store zen" $
      voom [(1, Just 2), (1, Just 5), (2, Just 3)]
        @?= []
  ]
