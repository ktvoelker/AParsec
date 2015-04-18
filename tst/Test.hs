
module Test where

import Test.QuickCheck.Property

import Text.Parsec.Applicative

import Types

isLeft, isRight :: Either a b -> Bool

isLeft (Left _) = True
isLeft (Right _) = False

isRight = not . isLeft

prop_theEndIsTheEnd :: P -> Tokens -> Bool
prop_theEndIsTheEnd p ts =
  case parse' p ts of
    (Right _, (_ : _)) -> isLeft $ fst ret'
    ret -> ret' == ret
  where
    ret' = parse' (p <* eof) ts

prop_repeatToken :: Int -> Sigma -> Bool
prop_repeatToken reps tok = many (token tok) `accept` zip (r tok) (r ())
  where
    r = replicate reps

prop_repeatTokens :: Int -> [Sigma] -> Property
prop_repeatTokens reps toks = not (null toks) ==> p `accept` ts
  where
    p = many . sequenceA . map token $ toks
    ts = zip (concat $ replicate reps toks) (repeat ())

prop_repeatTokensFailure :: Int -> [Sigma] -> Property
prop_repeatTokensFailure reps toks = length toks > 1 ==> not (p `accept` ts)
  where
    p = many . sequenceA . map token $ toks
    ts = zip (concat (replicate reps toks) ++ [head toks]) (repeat ())

prop_firstChoiceConsumeFail :: Sigma -> Sigma -> Property
prop_firstChoiceConsumeFail a b = a /= b ==> rp `accept` ts && not (p `accept` ts)
  where
    rp = sequenceA [token a, token b]
    p = sequenceA [token a, token a] <|> rp
    ts = zip [a, b] (repeat ())
