
{-# LANGUAGE TemplateHaskell #-}
module Text.Parsec.Applicative.Types where

import Data.Lens.Template

data ParseState tt td =
  ParseState
  { _psConsumed :: Bool
  , _psTokens :: [(tt, td)]
  } deriving (Show)

emptyParseState :: [(tt, td)] -> ParseState tt td
emptyParseState = ParseState False

makeLenses [''ParseState]

