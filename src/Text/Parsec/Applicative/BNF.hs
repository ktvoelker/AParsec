
module Text.Parsec.Applicative.BNF where

import qualified Data.Map as Map
import qualified Data.Text as T

import Text.Parsec.Applicative.Grammar
import Text.Parsec.Applicative.Internal

newtype BNF = BNF { bnfShowsBNF :: ShowS }

class ShowBNF a where
  showsBNF :: a -> String -> String

instance ShowBNF BNF where
  showsBNF = bnfShowsBNF

instance ShowBNF String where
  showsBNF xs = (xs ++)

instance ShowBNF T.Text where
  showsBNF xs = (T.unpack xs ++)

instance ShowBNF () where
  showsBNF = const id

instance Show BNF where
  showsPrec _ = showsBNF

grammarToBNF :: (ShowBNF s, ShowBNF t) => Grammar s t -> BNF
grammarToBNF = BNF . showsGrammarBNF

parserToBNF :: (ShowBNF s, ShowBNF tt, Ord s) => Parser s tt td a -> Maybe BNF
parserToBNF = fmap grammarToBNF . parserToGrammar

showsGrammarBNF :: (ShowBNF s, ShowBNF t) => Grammar s t -> ShowS
showsGrammarBNF (Grammar _ prods) = showsBNFProds prods

showsBNFProds :: (ShowBNF s, ShowBNF t) => Map.Map s (Expr s t) -> ShowS
showsBNFProds =
  flip (showsSepBy True) ("\n" ++)
  . map (\(n, e) -> showsBNF n . (" := " ++) . showsBNFExp True e)
  . Map.toList

showsBNFExp :: (ShowBNF s, ShowBNF t) => Bool -> Expr s t -> ShowS
showsBNFExp _ End = ("<End>" ++)
showsBNFExp _ Empty = ("ε" ++)
showsBNFExp _ (Terminal t) = showsBNF t
showsBNFExp _ (NonTerminal xs) = showsBNF xs
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

