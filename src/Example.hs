
module Example where

import Control.Applicative
import Data.Functor
import Data.Maybe

import Text.Parsec.Applicative
import Text.Parsec.Applicative.BNF

data TT = TTInt | TTPlus | TTMult | TTLP | TTRP
  deriving (Eq, Enum, Bounded, Show)

type TD = Maybe Integer

data Expr = Mult Expr Expr | Plus Expr Expr | Lit Integer
  deriving (Show)

lit = label "lit" $ Lit . fromJust . snd <$> token TTInt

factor = label "factor" $ f <$> lit <*> optional (token TTMult *> factor)
  where
    f e Nothing = e
    f e1 (Just e2) = Mult e1 e2

expr = label "expr" $
  f <$> factor <*> optional (token TTPlus *> expr)
  <|>
  token TTLP *> expr <* token TTRP
  where
    f e Nothing = e
    f e1 (Just e2) = Plus e1 e2

bnf = parserToBNF expr

