module Text.Parsec.Applicative.Grammar where

import Data.List
import qualified Data.Map as Map

import Text.Parsec.Applicative.Internal

data Grammar s t =
  Grammar
  { start :: s
  , productions :: Map.Map s (Expr s t)
  }

data Expr s t =
    End
  | Empty
  | Terminal t (Maybe s)
  | NonTerminal s
  | Sequence [Expr s t]
  | Choice [Expr s t]
  | Repeat (Expr s t)
  | Try (Expr s t)
  | Fail (Maybe String)

deriving instance (Show s, Show t) => Show (Expr s t)

deriving instance (Show s, Show t) => Show (Grammar s t)

nonTerminals
  :: (Eq s)
  => [(s, Parser s tt td ())]
  -> Parser s tt td a
  -> [(s, Parser s tt td ())]
nonTerminals acc PEnd = acc
nonTerminals acc (PConst _) = acc
nonTerminals acc (PToken _ _) = acc
nonTerminals acc (PSkip _ p) = nonTerminals acc p
nonTerminals acc (PApp f p) = nonTerminals (nonTerminals acc p) f
nonTerminals acc (PTry p) = nonTerminals acc p
nonTerminals acc (PRepeat p) = nonTerminals acc p
nonTerminals acc (PFail _) = acc
nonTerminals acc (PChoice p q) = nonTerminals (nonTerminals acc q) p
nonTerminals acc (PLabel xs p) = case find ((== xs) . fst) acc of
  Nothing -> nonTerminals ((xs, pure () <* p) : acc) p
  Just _ -> acc
nonTerminals acc PGetPos = acc

parserToGrammar :: (Ord s) => Parser s tt td a -> Maybe (Grammar s tt)
parserToGrammar p@(PLabel xs _) = Just (Grammar xs ps)
  where
    ps = Map.fromList . map (\(xs, p) -> (xs, flatten . ce $ p)) . nonTerminals [] $ p
parserToGrammar _ = Nothing

ce :: Parser s tt td a -> Expr s tt
ce PEnd          = End
ce (PConst _)    = Empty
ce (PToken t p)  = Terminal t (fmap predicateLabel p)
ce (PSkip p q)   = Sequence [ce p, ce q]
ce (PApp f p)    = Sequence [ce f, ce p]
ce (PTry p)      = Try (ce p)
ce (PRepeat p)   = Repeat (ce p)
ce (PFail xs)    = Fail xs
ce (PChoice f p) = Choice [ce f, ce p]
ce (PLabel xs _) = NonTerminal xs
ce PGetPos       = Empty

isFail :: Expr s t -> Bool
isFail (Fail _) = True
isFail _ = False

flatten :: Expr s t -> Expr s t
flatten e@End = e
flatten e@Empty = e
flatten e@(Terminal _ _) = e
flatten e@(NonTerminal _) = e
flatten e@(Fail _) = e
flatten e@(Sequence _) = case flattenSequence e of
  []  -> Empty
  [e] -> e
  es | any isFail es -> Fail Nothing
     | otherwise     -> Sequence es
flatten e@(Choice _) = case filter (not . isFail) $ flattenChoice e of
  []  -> Empty
  [e] -> e
  es  -> Choice es
flatten (Repeat e) = case flatten e of
  e@(Fail _) -> e
  e          -> Repeat e
flatten (Try e) = case flatten e of
  e@(Fail _) -> e
  e          -> Try e

flattenSequence :: Expr s t -> [Expr s t]
flattenSequence (Sequence es) = concatMap (flattenSequence . flatten) es
flattenSequence Empty = []
flattenSequence e = [flatten e]

flattenChoice :: Expr s t -> [Expr s t]
flattenChoice (Choice es) = concatMap (flattenChoice . flatten) es
flattenChoice e = [flatten e]

