
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
  . map (\(xs, p) -> (xs,) . flatten <$> ce p)
  . nonTerminals []

flatten :: BNFExp t -> BNFExp t
-- TODO
flatten = id

ce :: Parser tt td a -> Maybe (BNFExp tt)
ce PEnd = Just EOF
ce (PConst _) = Nothing
ce (PToken []) = Nothing
ce (PToken [t]) = Just (Token t)
ce (PToken ts) = Just . Alt . map Token $ ts
ce (PSkip p q) = Just . Seq . catMaybes $ [ce p, ce q]
ce (PApp f p) = Just . Seq . catMaybes $ [ce f, ce p]
ce (PTry p) = ce p
ce (PRepeat p) = Rep <$> ce p
ce (PFail _) = Nothing
ce (PChoice f p) = Just . Alt . catMaybes $ [ce f, ce p]
ce (PLabel xs _) = Just $ Ref xs

