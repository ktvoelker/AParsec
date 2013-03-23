
module Text.Parsec.Applicative where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State

data Parser tt td a where
  PConst  :: a -> Parser tt td a
  PToken  :: [tt] -> Parser tt td (tt, td)
  PSkip   :: Parser tt td a -> Parser tt td b -> Parser tt td b
  PApp    :: Parser tt td (a -> b) -> Parser tt td a -> Parser tt td b
  PTry    :: Parser tt td a -> Parser tt td a
  PRepeat :: Parser tt td a -> Parser tt td [a]
  PFail   :: Maybe String -> Parser tt td a
  PChoice :: Parser tt td a -> Parser tt td a -> Parser tt td a
  PInfo   :: String -> Parser tt td a -> Parser tt td a

instance Functor (Parser tt td) where
  fmap = PApp . PConst
  (<$) = flip PSkip . PConst

instance Applicative (Parser tt td) where
  pure  = PConst
  (<*>) = PApp
  (*>)  = PSkip
  (<*)  = flip PSkip

instance Alternative (Parser tt td) where
  empty  = PFail Nothing
  (<|>)  = PChoice
  some p = PApp (PApp (pure (:)) p) (PRepeat p)
  many p = PRepeat p

token :: (Eq tt, Enum tt, Bounded tt) => [tt] -> Parser tt td (tt, td)
token = PToken

data ParseError = ParseError

instance Error ParseError where
  noMsg    = ParseError
  strMsg _ = ParseError

parse
  :: (Eq tt, Enum tt, Bounded tt)
  => Parser tt td a -> [(tt, td)] -> Either ParseError a
parse p xs = evalState (runErrorT $ mp p) xs

type M t = ErrorT ParseError (State [t])

mp :: (Eq tt, Enum tt, Bounded tt) => Parser tt td a -> M t a
mp (PConst x) = return x
mp _ = undefined

