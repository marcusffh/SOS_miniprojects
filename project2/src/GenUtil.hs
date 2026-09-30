-- | A small deterministic random generator (SplitMix64) and combinators.
module MicroEbpf.GenUtil
  ( G
  , runG
  , word
  , range
  , oneOf
  , frequency
  , chance
  ) where

import Data.Bits (shiftR, xor)
import Data.Word (Word64)

newtype G a = G (Word64 -> (a, Word64))

instance Functor G where
  fmap f (G g) = G (\s -> let (a, s') = g s in (f a, s'))

instance Applicative G where
  pure a = G (\s -> (a, s))
  G gf <*> G ga = G (\s -> let (f, s1) = gf s
                               (a, s2) = ga s1
                           in (f a, s2))

instance Monad G where
  G g >>= k = G (\s -> let (a, s1) = g s
                           G h = k a
                       in h s1)

runG :: Word64 -> G a -> a
runG seed (G g) = fst (g seed)

-- | A uniformly distributed 64-bit word.
word :: G Word64
word = G (\s ->
  let s' = s + 0x9e3779b97f4a7c15
      z1 = (s' `xor` (s' `shiftR` 30)) * 0xbf58476d1ce4e5b9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
  in (z2 `xor` (z2 `shiftR` 31), s'))

-- | An integer in [lo, hi].
range :: Integer -> Integer -> G Integer
range lo hi
  | hi <= lo = pure lo
  | otherwise = do
      w <- word
      pure (lo + (toInteger w `mod` ((hi - lo) + 1)))

oneOf :: [a] -> G a
oneOf xs = do
  i <- range 0 (toInteger (length xs - 1))
  pure (xs !! fromInteger i)

-- | Choose a generator with probability proportional to its weight.
frequency :: [(Int, G a)] -> G a
frequency alts = do
  k <- range 1 (toInteger (sum (map fst alts)))
  pick (fromInteger k) alts
  where
    pick k ((w, g) : rest) = if k <= w then g else pick (k - w) rest
    pick _ [] = error "frequency: no alternatives"

-- | True with probability p/q.
chance :: Integer -> Integer -> G Bool
chance p q = do
  k <- range 1 q
  pure (k <= p)
