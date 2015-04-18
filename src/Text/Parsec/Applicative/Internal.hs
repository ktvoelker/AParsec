
module Text.Parsec.Applicative.Internal
  ( module Control.Applicative
  , module Text.Parsec.Applicative.Internal
  , module Text.Parsec.Applicative.Types
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Text as T

import Text.Parsec.Applicative.Types

import Debug.Trace

data Parser s tt td a where
  PEnd    :: Parser s tt td ()
  PConst  :: a -> Parser s tt td a
  PToken  :: tt -> Parser s tt td (tt, td)
  PSkip   :: Parser s tt td a -> Parser s tt td b -> Parser s tt td b
  PApp    :: Parser s tt td (a -> b) -> Parser s tt td a -> Parser s tt td b
  PTry    :: Parser s tt td a -> Parser s tt td a
  PRepeat :: Parser s tt td a -> Parser s tt td [a]
  PFail   :: Maybe String -> Parser s tt td a
  PChoice :: Parser s tt td a -> Parser s tt td a -> Parser s tt td a
  PLabel  :: s -> Parser s tt td a -> Parser s tt td a
  PGetPos :: (HasSourcePos td) => Parser s tt td SourcePos

parens :: ShowS -> ShowS
parens ss = ('(' :) . ss . (')' :)

listed :: [ShowS] -> ShowS
listed = parens . foldr (.) id . map1 ((' ' :) .)

map1 :: (a -> a) -> [a] -> [a]
map1 _ [] = []
map1 f (x : xs) = x : map f xs

instance (Show s, Show tt, Show td) => Show (Parser s tt td a) where
  showsPrec _ PEnd           = ("PEnd" ++)
  showsPrec _ (PConst _)     = ("PConst" ++)
  showsPrec p (PToken t)     = listed [("PToken" ++), showsPrec p t]
  showsPrec p (PSkip a b)    = listed [("PSkip" ++), showsPrec p a, showsPrec p b]
  showsPrec p (PApp a b)     = listed [("PApp" ++), showsPrec p a, showsPrec p b]
  showsPrec p (PTry a)       = listed [("PTry" ++), showsPrec p a]
  showsPrec p (PRepeat a)    = listed [("PRepeat" ++), showsPrec p a]
  showsPrec _ (PFail _)      = ("PFail" ++)
  showsPrec p (PChoice a b)  = listed [("PChoice" ++), showsPrec p a, showsPrec p b]
  showsPrec p (PLabel xs a ) = listed [("PLabel" ++), showsPrec p xs, showsPrec p a]
  showsPrec _ PGetPos        = ("PGetPos" ++)

instance Functor (Parser s tt td) where
  fmap = PApp . PConst

instance Applicative (Parser s tt td) where
  pure  = PConst
  (<*>) = PApp
  (*>)  = PSkip

instance Alternative (Parser s tt td) where
  empty  = PFail Nothing
  (<|>)  = PChoice
  some p = PApp (PApp (pure (:)) p) (PRepeat p)
  many p = PRepeat p

eof :: Parser s tt td ()
eof = PEnd

token :: (Eq tt) => tt -> Parser s tt td (tt, td)
token = PToken

try = PTry

label = PLabel

data ParseErrorType = EUnexpected | EEnd | ENotEnd
  deriving (Eq, Ord, Enum, Bounded, Show)

data ParseError =
  ParseError
  { peType      :: Maybe ParseErrorType
  , peMessage   :: Maybe T.Text
  , peSourcePos :: Maybe SourcePos
  } deriving (Eq, Show)

noMsg :: ParseError
noMsg  = ParseError Nothing Nothing Nothing

strMsg :: [Char] -> ParseError
strMsg = flip (ParseError Nothing) Nothing . Just . T.pack

data ParserError =
    ERepeatEmpty
  | EUnknown
  deriving (Eq, Show)

parse
  :: (Eq tt, HasSourcePos td)
  => Parser s tt td a -> [(tt, td)] -> Either ParseError a
parse = (fst .) . parse'

parse'
  :: (Eq tt, HasSourcePos td)
  => Parser s tt td a -> [(tt, td)] -> (Either ParseError a, [(tt, td)])
parse' p = (\(x, s) -> (x, s ^. psTokens)) . runM (mp p)

runM
  :: (Eq tt, HasSourcePos td)
  => M tt td a -> [(tt, td)] -> (Either ParseError a, ParseState tt td)
runM m = runState (runExceptT m) . emptyParseState

accept :: (Eq tt, HasSourcePos td) => Parser s tt td a -> [(tt, td)] -> Bool
accept = (either (const False) (const True) .) . parse

accept' :: (Eq tt, HasSourcePos td) => Parser s tt td a -> [(tt, td)] -> Maybe ParseError
accept' = (either Just (const Nothing) .) . parse

data Ex f = forall a. Ex (f a)

acceptEmpty :: Ex (Parser s tt td) -> Bool
acceptEmpty (Ex PEnd) = True
acceptEmpty (Ex (PConst _)) = True
acceptEmpty (Ex (PToken _)) = False
acceptEmpty (Ex (PSkip a b)) = all acceptEmpty [Ex a, Ex b]
acceptEmpty (Ex (PApp a b)) = all acceptEmpty [Ex a, Ex b]
acceptEmpty (Ex (PTry a)) = acceptEmpty (Ex a)
acceptEmpty (Ex (PRepeat _)) = True
acceptEmpty (Ex (PFail _)) = False
acceptEmpty (Ex (PChoice a b)) = any acceptEmpty [Ex a, Ex b]
acceptEmpty (Ex (PLabel _ a)) = acceptEmpty (Ex a)
acceptEmpty (Ex PGetPos) = True

validate :: Parser s tt td a -> [(ParserError, String)]
validate = execWriter . f
  where
    f _ = undefined

infix 4 `accept`, `accept'`

localConsumption :: M tt td a -> M tt td a
localConsumption p = do
  con <- use psConsumed
  assign psConsumed False
  ret <- p
  psConsumed %= (|| con)
  return ret

type M tt td = ExceptT ParseError (State (ParseState tt td))

mp :: (Eq tt, HasSourcePos td) => Parser s tt td a -> M tt td a
mp PEnd = use psTokens >>= \case
  [] -> return ()
  (_, td) : _ -> throwError . ParseError (Just ENotEnd) Nothing . Just . sourcePos $ td
mp (PConst x) = return x
mp (PToken exp) = use psTokens >>= \case
  t@(act, _) : ts | exp == act -> do
    assign psConsumed True
    assign psTokens ts
    return t
  (_, td) : _ -> throwError . ParseError Nothing Nothing . Just . sourcePos $ td
  _ -> throwError $ ParseError (Just EEnd) Nothing Nothing
mp (PSkip p1 p2) = mp p1 >> mp p2
mp (PApp f a) = mp f <*> mp a
mp (PTry p) = do
  ts <- get
  catchError (mp p) $ \err -> do
    put ts
    throwError err
mp (PRepeat p) = mp $ ((:) <$> p <*> PRepeat p) <|> PConst []
mp (PFail Nothing) = throwError noMsg
mp (PFail (Just xs)) = throwError $ strMsg xs
mp (PChoice p1 p2) = do
  localConsumption
  . catchError (mp p1)
  $ \err -> use psConsumed >>= \case
    True  -> throwError err
    -- TODO if p2 throws an error, there might be some merging to do with p1's error
    False -> mp p2
-- TODO simplify error messages that bubble up through here
mp (PLabel _ p) = mp p
mp PGetPos = use psTokens >>= return . \case
  [] -> noPos
  (_, td) : _ -> sourcePos td

