
module Types (module Text.Parsec.Applicative, module Types) where

import Data.Monoid
import Test.QuickCheck

import Text.Parsec.Applicative
import Text.Parsec.Applicative.Internal

type Tokens = [(Sigma, Extra)]

data Sigma = A | B | C deriving (Eq, Ord, Enum, Bounded, Show)

instance Arbitrary Sigma where
  arbitrary = arbitraryBoundedEnum

data Extra = E | F | G deriving (Eq, Ord, Enum, Bounded, Show)

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

type P = Parser String Sigma Extra Extra

newtype RecP = RecP { getP :: P }

instance Arbitrary P where
  arbitrary = arbitraryParser False

instance Arbitrary RecP where
  arbitrary = RecP <$> arbitraryParser True

arbitraryParser :: Bool -> Gen P
arbitraryParser rec = sized $ arb rec

arb :: Bool -> Int -> Gen P
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

