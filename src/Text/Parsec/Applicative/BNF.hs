
module Text.Parsec.Applicative.BNF where

import qualified Data.Map as Map
import Data.List
import Data.Maybe

import Text.Parsec.Applicative.Internal

type BNF t = Map.Map String (BNFExp t)

data BNFExp t =
    EOF
  | Empty
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
flatten e@EOF = e
flatten e@Empty = e
flatten e@(Token _) = e
flatten e@(Ref _) = e
flatten e@(Seq _) = case flattenSeq e of
  []  -> Empty
  [e] -> e
  es  -> Seq es
flatten e@(Alt _) = case flattenAlt e of
  []  -> Empty
  [e] -> e
  es  -> Alt es
flatten (Rep e) = Rep $ flatten e

flattenSeq :: BNFExp t -> [BNFExp t]
flattenSeq (Seq es) = concatMap (flattenSeq . flatten) es
flattenSeq Empty = []
flattenSeq e = [flatten e]

flattenAlt :: BNFExp t -> [BNFExp t]
flattenAlt (Alt es) = concatMap (flattenAlt . flatten) es
flattenAlt e = [flatten e]

ce :: Parser tt td a -> Maybe (BNFExp tt)
ce PEnd = Just EOF
ce (PConst _) = Just Empty
ce (PToken t) = Just (Token t)
ce (PSkip p q) = Just . Seq . catMaybes $ [ce p, ce q]
ce (PApp f p) = Just . Seq . catMaybes $ [ce f, ce p]
ce (PTry p) = ce p
ce (PRepeat p) = Rep <$> ce p
ce (PFail _) = Nothing
ce (PChoice f p) = Just . Alt . catMaybes $ [ce f, ce p]
ce (PLabel xs _) = Just $ Ref xs

showsBNF :: (Show t) => BNF t -> ShowS
showsBNF =
  flip (showsSepBy True) ("\n" ++)
  . map (\(n, e) -> (n ++) . (" := " ++) . showsBNFExp True e)
  . Map.toList

showsBNFExp :: (Show t) => Bool -> BNFExp t -> ShowS
showsBNFExp _ EOF = ("<EOF>" ++)
showsBNFExp _ Empty = ("ε" ++)
showsBNFExp _ (Token t) = shows t
showsBNFExp _ (Ref xs) = (xs ++)
showsBNFExp safe (Seq es) = showsSepBy safe (map (showsBNFExp False) es) (" " ++)
showsBNFExp safe (Alt es) = showsSepBy safe (map (showsBNFExp True) es) (" | " ++)
showsBNFExp _ (Rep e) = showsBNFExp False e . ("*" ++)

showsParens :: ShowS -> ShowS
showsParens f = ("(" ++) . f . (")" ++)

showsSepBy :: Bool -> [ShowS] -> ShowS -> ShowS
showsSepBy _ [] _ = id
showsSepBy _ [s] _ = s
showsSepBy safe (s : ss) sep =
  (if safe then id else showsParens)
  $ foldl (.) s $ map (sep .) ss

