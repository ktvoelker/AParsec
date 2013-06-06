
module Main where

import Test.Framework (Test(), defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Test

tests :: [Test]
tests =
  [ testGroup "Simple"
    [ testProperty "Constant true" Test.prop_true
    , testProperty "EOF matches only at the end" Test.prop_theEndIsTheEnd
    , testProperty "Concatenation with (<*) and (*>)" Test.prop_skip
    , testProperty "Repeating a token" Test.prop_repeatToken
    , testProperty "Repeating a token sequence" Test.prop_repeatTokens
    ]
  ]

main :: IO ()
main = defaultMain tests

