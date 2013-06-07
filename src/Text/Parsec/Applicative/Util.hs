
module Text.Parsec.Applicative.Util where

sequenceJust :: (Monad m) => [m (Maybe a)] -> m [a]
sequenceJust = f id
  where
    f k [] = done k
    f k (m : ms) =
      f (\ms' -> m >>=
        \x -> maybe (done k) (\x' -> ms' >>= \xs' -> k $ return (x' : xs')) x) ms
    done = ($ return [])

