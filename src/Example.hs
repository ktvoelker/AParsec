
module Example where

import Control.Applicative
import Data.Functor
import Data.Maybe
import Text.Parsec.Applicative

data TT = TTInt | TTPlus | TTMult | TTLP | TTRP
  deriving (Eq, Enum, Bounded)

type TD = Maybe Integer

data Expr = Mult Expr Expr | Plus Expr Expr | Lit Integer
  deriving (Show)

lit = Lit . fromJust . snd <$> token [TTInt]

factor = f <$> lit <*> optional (token [TTMult] *> factor)
  where
    f e Nothing = e
    f e1 (Just e2) = Mult e1 e2

expr =
  f <$> factor <*> optional (token [TTPlus] *> expr)
  <|>
  token [TTLP] *> expr <* token [TTRP]
  where
    f e Nothing = e
    f e1 (Just e2) = Plus e1 e2

