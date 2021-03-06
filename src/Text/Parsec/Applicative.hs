module Text.Parsec.Applicative
  ( module Control.Applicative
  , module Data.Traversable
  , module Text.Parsec.Applicative.Types
  , Parser()
  , ParseError(..)
  , ParseErrorType(..)
  , predicate
  , eof
  , token
  , token'
  , try
  , label
  , parse
  , parse'
  , accept
  , accept'
  , between
  , choice
  , option
  , sepBy
  , updatePosString
  , getPosition
  ) where

import Control.Applicative
import Data.Traversable (Traversable(traverse, sequenceA), for, mapAccumL, mapAccumR)
import Lens.Micro

import Text.Parsec.Applicative.Internal
import Text.Parsec.Applicative.Types

between :: (Applicative f) => f a -> f b -> f c -> f c
between l r m = l *> m <* r

choice :: (Alternative f) => [f a] -> f a
choice = foldr (<|>) empty

option :: (Alternative f) => f a -> f (Maybe a)
option p = (Just <$> p) <|> pure Nothing

sepBy :: (Alternative f) => f a -> f b -> f [a]
sepBy p delim = ((:) <$> p <*> many (delim *> p)) <|> empty

updatePosString :: SourcePos -> String -> SourcePos
updatePosString = foldr f
  where
    f '\n' = (set spColumn 0) . (over spLine (+ 1))
    f _ = over spColumn (+ 1)

getPosition :: (HasSourcePos td) => Parser s tt td SourcePos
getPosition = PGetPos

