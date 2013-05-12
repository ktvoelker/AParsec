
module Types (module Text.Parsec.Applicative, module Types) where

import Data.Monoid
import Test.QuickCheck

import Text.Parsec.Applicative
import Text.Parsec.Applicative.Internal

data Sigma = A | B | C deriving (Eq, Ord, Enum, Bounded)

instance Arbitrary Sigma where
  arbitrary = arbitraryBoundedEnum

data Extra = E | F | G deriving (Eq, Ord, Enum, Bounded)

instance Monoid Extra where
  mempty = E
  mappend E x = x
  mappend x E = x
  mappend F F = G
  mappend F G = E
  mappend G F = E
  mappend G G = F

instance Arbitrary Extra where
  arbitrary = arbitraryBoundedEnum

type StringParser = Parser Sigma Extra ()

type PrefixParser = Parser Sigma Extra Extra

newtype RecStringParser = RecStringParser { getStringParser :: StringParser }

newtype RecPrefixParser = RecPrefixParser { getPrefixParser :: PrefixParser }

terminate :: Gen (Parser tt td a) -> Gen (Parser tt td ())
terminate p = PSkip <$> p <*> pure PEnd

instance Arbitrary StringParser where
  arbitrary = terminate (arbitrary :: Gen PrefixParser)

instance Arbitrary PrefixParser where
  arbitrary = arbitraryParser False

instance Arbitrary RecStringParser where
  arbitrary = RecStringParser <$> terminate (getPrefixParser <$> arbitrary)

instance Arbitrary RecPrefixParser where
  arbitrary = RecPrefixParser <$> arbitraryParser True

arbitraryParser :: Bool -> Gen PrefixParser
arbitraryParser rec = sized $ arb rec

arb :: Bool -> Int -> Gen PrefixParser
arb _ 0 = oneof
  [ (PEnd *>) . PConst <$> arbitrary
  , PConst <$> arbitrary
  , PApp (pure snd) . PToken <$> arbitrary
  , return $ PFail $ Just "arbitrary"
  ]
arb rec n = oneof
  [ bin PSkip
  -- TODO generate real functions
  , PApp <$> ((*> pure id) <$> arb') <*> arb'
  , PTry <$> arb'
  , PApp (pure mconcat) . PRepeat <$> arb'
  , bin PChoice
  -- TODO generate recursive parsers
  , PLabel "label" <$> arb'
  ]
  where
    arb' = arb rec $ n `div` 2
    bin op = op <$> arb' <*> arb'

