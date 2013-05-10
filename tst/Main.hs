
module Main where

import Data.Functor
import Data.List
import System.Exit
import Test.QuickCheck.Test

import qualified Test

tests :: IO [Result]
tests = mapM quickCheckResult [Test.empty]

main :: IO ()
main = (partition isSuccess <$> tests) >>= \case
  (ss, fs) -> do
    let ns = length ss
    let nf = length fs
    putStrLn $ shows ns . (" passed; " ++) . shows nf . (" failed." ++) $ ""
    case nf of
      0 -> exitSuccess
      _ -> exitFailure

