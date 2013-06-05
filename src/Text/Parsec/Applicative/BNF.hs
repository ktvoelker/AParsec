
module Text.Parsec.Applicative.BNF where

import qualified Data.Map as Map

import Text.Parsec.Applicative.Grammar
import Text.Parsec.Applicative.Internal

newtype BNF = BNF { showsPrecBNF :: Int -> ShowS }

instance Show BNF where
  showsPrec n bnf = showsPrecBNF bnf n

grammarToBNF :: (Show s, Show t) => Grammar s t -> BNF
grammarToBNF = BNF . const . showsGrammarBNF

parserToBNF :: (Show s, Show tt, Ord s) => Parser s tt td a -> Maybe BNF
parserToBNF = fmap grammarToBNF . parserToGrammar

showsGrammarBNF :: (Show s, Show t) => Grammar s t -> ShowS
showsGrammarBNF (Grammar start prods) = showsBNFStart start . showsBNFProds prods

showsBNFStart :: (Show s) => s -> ShowS
showsBNFStart xs = shows xs . ("\n" ++)

showsBNFProds :: (Show s, Show t) => Map.Map s (Expr s t) -> ShowS
showsBNFProds =
  flip (showsSepBy True) ("\n" ++)
  . map (\(n, e) -> shows n . (" := " ++) . showsBNFExp True e)
  . Map.toList

showsBNFExp :: (Show s, Show t) => Bool -> Expr s t -> ShowS
showsBNFExp _ End = ("<End>" ++)
showsBNFExp _ Empty = ("ε" ++)
showsBNFExp _ (Terminal t) = shows t
showsBNFExp _ (NonTerminal xs) = shows xs
showsBNFExp safe (Sequence es) = showsSepBy safe (map (showsBNFExp False) es) (" " ++)
showsBNFExp safe (Choice es) = showsSepBy safe (map (showsBNFExp True) es) (" | " ++)
showsBNFExp _ (Repeat e) = showsBNFExp False e . ("*" ++)
showsBNFExp _ (Try e) = ("TRY" ++) . showsParens (showsBNFExp True e)
showsBNFExp _ (Fail xs) = ("<Fail" ++) . maybe id (((": " ++) .) . (++)) xs . (">" ++)

showsParens :: ShowS -> ShowS
showsParens f = ("(" ++) . f . (")" ++)

showsSepBy :: Bool -> [ShowS] -> ShowS -> ShowS
showsSepBy _ [] _ = id
showsSepBy _ [s] _ = s
showsSepBy safe (s : ss) sep =
  (if safe then id else showsParens)
  $ foldl (.) s $ map (sep .) ss

