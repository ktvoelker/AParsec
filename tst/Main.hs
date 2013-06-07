
module Main where

import Data.Monoid
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import System.Environment

import qualified Test

tests :: [Test]
tests =
  [ testGroup "Simple"
    [ testProperty "Constant true" Test.prop_true
    -- , testProperty "EOF matches only at the end" Test.prop_theEndIsTheEnd
    -- , testProperty "Concatenation with (<*) and (*>)" Test.prop_skip
    , testProperty "Repeating a token" Test.prop_repeatToken
    -- , testProperty "Repeating a token sequence" Test.prop_repeatTokens
    -- , testProperty "Compare sequenceJust to reference" Test.prop_sequenceJust
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
            = Just 64
          }
      }

