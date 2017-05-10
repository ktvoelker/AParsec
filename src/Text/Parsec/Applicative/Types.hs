{-# LANGUAGE TemplateHaskell #-}
module Text.Parsec.Applicative.Types
  ( SourcePos(SourcePos)
  , spName
  , spLine
  , spColumn
  , initialPos
  , noPos
  , HasSourcePos(..)
  , WithSourcePos(WithSourcePos)
  , wspValue
  , wspSourcePos
  , ParseState()
  , psConsumed
  , psTokens
  , emptyParseState
  ) where

import qualified Data.Text as T
import Lens.Micro.TH

data SourcePos =
  SourcePos
  { _spName   :: Maybe T.Text
  , _spLine   :: Integer
  , _spColumn :: Integer
  } deriving (Eq, Ord, Show)

makeLenses ''SourcePos

initialPos :: Maybe T.Text -> SourcePos
initialPos name = SourcePos name 1 1

noPos :: SourcePos
noPos = SourcePos Nothing (-1) (-1)

class HasSourcePos a where
  sourcePos :: a -> SourcePos

data WithSourcePos a =
  WithSourcePos
  { _wspValue     :: a
  , _wspSourcePos :: SourcePos
  } deriving (Eq, Ord, Show)

makeLenses ''WithSourcePos

instance HasSourcePos SourcePos where
  sourcePos = id

instance HasSourcePos (WithSourcePos a) where
  sourcePos = _wspSourcePos

data ParseState tt td =
  ParseState
  { _psConsumed :: Bool
  , _psTokens :: [(tt, td)]
  } deriving (Show)

makeLenses ''ParseState

emptyParseState :: (HasSourcePos td) => [(tt, td)] -> ParseState tt td
emptyParseState = ParseState False

