
module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import System.Environment

import qualified Test

tests :: [Test]
tests =
  [ testGroup "Simple"
    [ testProperty "EOF matches only at the end" Test.prop_theEndIsTheEnd
    , testProperty "Repeating a token" Test.prop_repeatToken
    , testProperty "Repeating a token sequence" Test.prop_repeatTokens
    , testProperty
      "Repeating a token sequence with incomplete last repetition"
      Test.prop_repeatTokensFailure
    , testProperty
      "Choice where the first branch fails after consuming input"
      Test.prop_firstChoiceConsumeFail 
    ]
  ]

main :: IO ()
main = getArgs >>= interpretArgsOrExit >>= defaultMainWithOpts tests . f
  where
    f :: RunnerOptions -> RunnerOptions
    f ro = ro
      { ropt_test_options
        = Just $ (maybe mempty id $ ropt_test_options ro)
          { topt_maximum_test_size
            = Just 16
          }
      }

