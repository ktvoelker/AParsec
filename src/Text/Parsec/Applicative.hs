
module Text.Parsec.Applicative where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Data.Lens
import Data.Maybe

import Text.Parsec.Applicative.Types

data Parser tt td a where
  PEnd    :: Parser tt td ()
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

eof :: Parser tt td ()
eof = PEnd

token :: (Eq tt, Enum tt, Bounded tt) => [tt] -> Parser tt td (tt, td)
token = PToken

data ParseError = ParseError

instance Error ParseError where
  noMsg    = ParseError
  strMsg _ = ParseError

parse
  :: (Eq tt, Enum tt, Bounded tt)
  => Parser tt td a -> [(tt, td)] -> Either ParseError a
parse p = evalState (runErrorT $ mp p) . emptyParseState

localConsumption :: M tt td a -> M tt td a
localConsumption p = do
  con <- psConsumed ~= False
  ret <- p
  _   <- psConsumed %= (|| con)
  return ret

type M tt td = ErrorT ParseError (State (ParseState tt td))

mp :: (Eq tt, Enum tt, Bounded tt) => Parser tt td a -> M tt td a
mp PEnd = access psTokens >>= \case
  [] -> return ()
  _ -> throwError ParseError
mp (PConst x) = return x
mp (PToken tts) = access psTokens >>= \case
  [] -> throwError ParseError
  (t@(tt, _) : ts) | tt `elem` tts -> psTokens ~= ts >> return t
  _ -> throwError ParseError
mp (PSkip p1 p2) = mp p1 >> mp p2
mp (PApp f a) = mp f <*> mp a
mp (PTry p) = do
  ts <- get
  catchError (mp p) $ \err -> do
    put ts
    throwError err
-- TODO this could be implemented in terms of PChoice
mp (PRepeat p) =
  fmap (catMaybes . takeWhile isJust)
  . sequence
  . repeat
  . localConsumption
  . catchError (Just <$> mp p)
  $ \err -> access psConsumed >>= \case
    True  -> throwError err
    False -> return Nothing
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
mp (PInfo _ p) = mp p

