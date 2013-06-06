
module Test where

import Test.QuickCheck.Gen (Gen())
import Test.QuickCheck.Property

import Text.Parsec.Applicative

import Types

prop_true :: Bool
prop_true = True

isLeft, isRight :: Either a b -> Bool

isLeft (Left _) = True
isLeft (Right _) = False

isRight = not . isLeft

-- TODO this property is probably false due to backtracking
prop_theEndIsTheEnd :: P -> Tokens -> Bool
prop_theEndIsTheEnd p ts =
  case parse' p ts of
    (Right _, (_ : _)) -> isLeft . fst $ parse' (p <* eof) ts
    ret -> parse' (p <* eof) ts == ret

prop_skip :: P -> Tokens -> P -> Tokens -> Gen Prop
prop_skip p1 t1 p2 t2 =
  p1 `accept` t1 && p2 `accept` t2
  ==>
  (p1 <* p2) `accept` t12
    && (p1 *> p2) `accept` t12
    && (p2 <* p1) `accept` t21
    && (p2 *> p1) `accept` t21
  where
    t12 = t1 ++ t2
    t21 = t2 ++ t1

prop_repeatToken :: Int -> Sigma -> Bool
prop_repeatToken reps tok = many (token tok) `accept` zip (r tok) (r ())
  where
    r = replicate reps

prop_repeatTokens :: Int -> [Sigma] -> Bool
prop_repeatTokens reps toks = p `accept` ts
  where
    p = many . sequenceA . map token $ toks
    ts = zip (concat $ replicate reps toks) (repeat ())

