
module Text.Parsec.Applicative.Internal
  ( module Control.Applicative
  , module Text.Parsec.Applicative.Internal
  ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import Data.Lens

import Text.Parsec.Applicative.Types

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

data ParseError = ParseError
  deriving (Eq, Show)

instance Error ParseError where
  noMsg    = ParseError
  strMsg _ = ParseError

data ParserError =
    ERepeatEmpty
  | EUnknown
  deriving (Eq, Show)

parse
  :: (Eq tt)
  => Parser s tt td a -> [(tt, td)] -> Either ParseError a
parse = (fst .) . parse'

parse'
  :: (Eq tt)
  => Parser s tt td a -> [(tt, td)] -> (Either ParseError a, [(tt, td)])
parse' p = (\(x, s) -> (x, psTokens ^$ s)) . runM (mp p)

runM
  :: (Eq tt)
  => M tt td a -> [(tt, td)] -> (Either ParseError a, ParseState tt td)
runM m = runState (runErrorT m) . emptyParseState

accept :: (Eq tt) => Parser s tt td a -> [(tt, td)] -> Bool
accept = (either (const False) (const True) .) . parse

accept' :: (Eq tt) => Parser s tt td a -> [(tt, td)] -> Maybe ParseError
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

validate :: Parser s tt td a -> [(ParserError, String)]
validate = execWriter . f
  where
    f _ = undefined

infix 4 `accept`, `accept'`

localConsumption :: M tt td a -> M tt td a
localConsumption p = do
  con <- psConsumed ~= False
  ret <- p
  _   <- psConsumed %= (|| con)
  return ret

type M tt td = ErrorT ParseError (State (ParseState tt td))

mp :: (Eq tt) => Parser s tt td a -> M tt td a
mp PEnd = access psTokens >>= \case
  [] -> return ()
  _ -> throwError ParseError
mp (PConst x) = return x
mp (PToken exp) = access psTokens >>= \case
  t@(act, _) : ts | exp == act -> psTokens ~= ts >> return t
  _ -> throwError ParseError
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
  $ \err -> access psConsumed >>= \case
    True  -> throwError err
    -- TODO if p2 throws an error, there might be some merging to do with p1's error
    False -> mp p2
-- TODO simplify error messages that bubble up through here
mp (PLabel _ p) = mp p

