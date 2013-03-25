
module Text.Parsec.Applicative.BNF where

import Text.Parsec.Applicative.Internal

data BNF t =
    EOF
  | Token t
  | Seq [BNF t]
  | Alt [BNF t]
  | Rep (BNF t)

deriving instance (Show t) => Show (BNF t)

parserToBNF :: Parser tt td a -> Maybe (BNF tt)
parserToBNF PEnd = Just EOF
parserToBNF (PConst _) = Nothing
parserToBNF (PToken []) = Nothing
parserToBNF (PToken [t]) = Just (Token t)
parserToBNF (PToken ts) = Just . Alt . map Token $ ts
parserToBNF (PSkip _ p) = parserToBNF p
parserToBNF (PApp f p) = case (parserToBNF f, parserToBNF p) of
  (Nothing, y) -> y
  (x, Nothing) -> x
  (Just (Seq xs), Just (Seq ys)) -> Just . Seq $ xs ++ ys
  (Just (Seq xs), Just y) -> Just . Seq $ xs ++ [y]
  (Just x, Just (Seq ys)) -> Just . Seq $ [x] ++ ys
  (Just x, Just y) -> Just $ Seq [x, y]
parserToBNF (PTry p) = parserToBNF p
parserToBNF (PRepeat p) = Rep <$> parserToBNF p
parserToBNF (PFail _) = Nothing
parserToBNF (PChoice p q) = case (parserToBNF p, parserToBNF q) of
  (Nothing, y) -> y
  (x, Nothing) -> x
  (Just (Alt xs), Just (Alt ys)) -> Just . Alt $ xs ++ ys
  (Just (Alt xs), Just y) -> Just . Alt $ xs ++ [y]
  (Just x, Just (Alt ys)) -> Just . Alt $ [x] ++ ys
  (Just x, Just y) -> Just $ Alt [x, y]
parserToBNF (PInfo _ p) = parserToBNF p

