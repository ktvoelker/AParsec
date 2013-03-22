
{-# LANGUAGE GADTs #-}
module AParsec where

import Control.Applicative

data Parser t a where
  PConst  :: a -> Parser t a
  PToken  :: t -> Parser t t
  PSkip   :: Parser t a -> Parser t b -> Parser t b
  PApp    :: Parser t (a -> b) -> Parser t a -> Parser t b
  PTry    :: Parser t a -> Parser t a
  PRepeat :: Parser t a -> Parser t [a]
  PFail   :: Parser t a
  PChoice :: Parser t a -> Parser t a -> Parser t a

instance Functor (Parser t) where
  fmap = PApp . PConst
  (<$) = flip PSkip . PConst

instance Applicative (Parser t) where
  pure  = PConst
  (<*>) = PApp
  (*>)  = PSkip
  (<*)  = flip PSkip

instance Alternative (Parser t) where
  empty  = PFail
  (<|>)  = PChoice
  some p = PApp (PApp (pure (:)) p) (PRepeat p)
  many p = PRepeat p

