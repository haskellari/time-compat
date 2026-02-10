{-# LANGUAGE TemplateHaskell #-}

module Test.Clock.Lift
  ( testLift,
  )
where

import Data.Time.Clock.Compat
import qualified Language.Haskell.TH.Syntax as TH
import Test.Tasty
import Test.Tasty.HUnit

testLift :: TestTree
testLift =
  testGroup
    "Lift instances"
    [ testCase "DiffTime" $ $(TH.lift (secondsToDiffTime 100)) @?= secondsToDiffTime 100,
      testCase "NominalDiffTime" $ $(TH.lift (secondsToNominalDiffTime 100)) @?= secondsToNominalDiffTime 100
    ]
