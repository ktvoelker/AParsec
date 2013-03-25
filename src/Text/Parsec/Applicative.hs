
module Text.Parsec.Applicative
  ( module Control.Applicative
  , Parser()
  , ParseError(..)
  , eof
  , token
  , try
  ) where

import Control.Applicative

import Text.Parsec.Applicative.BNF
import Text.Parsec.Applicative.Internal

instance (Show tt) => Show (Parser tt td a) where
  showsPrec prec = showsPrec prec . parserToBNF

