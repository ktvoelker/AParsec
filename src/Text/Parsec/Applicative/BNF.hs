
module Text.Parsec.Applicative.BNF where

import qualified Data.Map as Map
import Data.List
import Data.Maybe

import Text.Parsec.Applicative.Internal

type BNF t = Map.Map String (BNFExp t)

data BNFExp t =
    EOF
  | Token t
  | Ref String
  | Seq [BNFExp t]
  | Alt [BNFExp t]
  | Rep (BNFExp t)

deriving instance (Show t) => Show (BNFExp t)

nonTerminals
  :: [(String, Parser tt td ())]
  -> Parser tt td a
  -> [(String, Parser tt td ())]
nonTerminals acc PEnd = acc
nonTerminals acc (PConst _) = acc
nonTerminals acc (PToken _) = acc
nonTerminals acc (PSkip _ p) = nonTerminals acc p
nonTerminals acc (PApp f p) = nonTerminals (nonTerminals acc p) f
nonTerminals acc (PTry p) = nonTerminals acc p
nonTerminals acc (PRepeat p) = nonTerminals acc p
nonTerminals acc (PFail _) = acc
nonTerminals acc (PChoice p q) = nonTerminals (nonTerminals acc q) p
nonTerminals acc (PLabel xs p) = case find ((== xs) . fst) acc of
  Nothing -> nonTerminals ((xs, pure () <* p) : acc) p
  Just _ -> acc

parserToBNF :: Parser tt td a -> BNF tt
parserToBNF =
  Map.fromList
  . catMaybes
  . map (\(xs, p) -> (xs,) <$> ce p)
  . nonTerminals []

ce :: Parser tt td a -> Maybe (BNFExp tt)
ce PEnd = Just EOF
ce (PConst _) = Nothing
ce (PToken []) = Nothing
ce (PToken [t]) = Just (Token t)
ce (PToken ts) = Just . Alt . map Token $ ts
ce (PSkip _ p) = ce p
ce (PApp f p) = case (ce f, ce p) of
  (Nothing, y) -> y
  (x, Nothing) -> x
  (Just (Seq xs), Just (Seq ys)) -> Just . Seq $ xs ++ ys
  (Just (Seq xs), Just y) -> Just . Seq $ xs ++ [y]
  (Just x, Just (Seq ys)) -> Just . Seq $ [x] ++ ys
  (Just x, Just y) -> Just $ Seq [x, y]
ce (PTry p) = ce p
ce (PRepeat p) = Rep <$> ce p
ce (PFail _) = Nothing
ce (PChoice p q) = case (ce p, ce q) of
  (Nothing, y) -> y
  (x, Nothing) -> x
  (Just (Alt xs), Just (Alt ys)) -> Just . Alt $ xs ++ ys
  (Just (Alt xs), Just y) -> Just . Alt $ xs ++ [y]
  (Just x, Just (Alt ys)) -> Just . Alt $ [x] ++ ys
  (Just x, Just y) -> Just $ Alt [x, y]
ce (PLabel xs _) = Just $ Ref xs

