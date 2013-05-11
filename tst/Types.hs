
module Types (module Text.Parsec.Applicative, module Types) where

import Test.QuickCheck

import Text.Parsec.Applicative
import Text.Parsec.Applicative.Internal

data Sigma = A | B | C deriving (Eq, Ord, Enum, Bounded)

instance Arbitrary Sigma where
  arbitrary = arbitraryBoundedEnum

data Extra = E | F | G deriving (Eq, Ord, Enum, Bounded)

instance Arbitrary Extra where
  arbitrary = arbitraryBoundedEnum

type StringParser = Parser Sigma Extra ()

type PrefixParser = Parser Sigma Extra Extra

instance Arbitrary StringParser where
  arbitrary = PSkip <$> (arbitrary :: Gen PrefixParser) <*> pure PEnd

instance Arbitrary PrefixParser where
  -- TODO
  arbitrary = oneof []

