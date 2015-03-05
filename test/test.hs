
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

------------------------------------------------------------

main = defaultMain tests

------------------------------------------------------------

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

voom :: [a] -> [a]
voom _ = []

unitTests = testGroup "Unit tests"
  [ testCase "Simplify renamings" $
      voom ([] :: [(Int, Maybe Int)])
        @?= []
  , testCase "Simplify renamings" $
      voom [(1, Just 2), (2, Just 3)]
        @?= []
  , testCase "Simplify renamings" $
      voom [(1, Just 2), (7, Just 8), (2, Just 3)]
        @?= []
  , testCase "Simplify renamings" $
      voom [(1, Just 2), (1, Just 5), (2, Just 3)]
        @?= []
  ]
