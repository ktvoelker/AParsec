
{-# LANGUAGE GADTs #-}
module AParsec where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State

data Parser t a where
  PConst  :: a -> Parser t a
  PToken  :: (t -> Bool) -> Parser t t
  PSkip   :: Parser t a -> Parser t b -> Parser t b
  PApp    :: Parser t (a -> b) -> Parser t a -> Parser t b
  PTry    :: Parser t a -> Parser t a
  PRepeat :: Parser t a -> Parser t [a]
  PFail   :: Maybe String -> Parser t a
  PChoice :: Parser t a -> Parser t a -> Parser t a
  PInfo   :: String -> Parser t a -> Parser t a

instance Functor (Parser t) where
  fmap = PApp . PConst
  (<$) = flip PSkip . PConst

instance Applicative (Parser t) where
  pure  = PConst
  (<*>) = PApp
  (*>)  = PSkip
  (<*)  = flip PSkip

instance Alternative (Parser t) where
  empty  = PFail Nothing
  (<|>)  = PChoice
  some p = PApp (PApp (pure (:)) p) (PRepeat p)
  many p = PRepeat p

data ParseError = ParseError

instance Error ParseError where
  noMsg    = ParseError
  strMsg _ = ParseError

parse :: Parser t a -> [t] -> Either ParseError a
parse p xs = evalState (runErrorT $ mp p) xs

type M t = ErrorT ParseError (State [t])

mp :: Parser t a -> M t a
mp (PConst x) = return x
mp _ = undefined

