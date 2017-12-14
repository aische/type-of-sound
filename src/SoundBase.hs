module SoundBase where

import System.Random
import System.IO.Unsafe
import WavFile

phasor :: [Double] -> [Double]
phasor = scanl (+) 0

sine :: [Double] -> [Double]
sine = map sin

mult :: [Double] -> [Double] -> [Double]
mult = zipWith (*)

add :: [Double] -> [Double] -> [Double]
add = zipWith (+)

sub :: [Double] -> [Double] -> [Double]
sub = zipWith (-)

constant :: Double -> [Double]
constant d = repeat d

neg :: [Double] -> [Double]
neg = map negate

lowpass :: [Double] -> [Double] -> [Double]
lowpass bs xs = loop 0 bs xs
  where
    loop y (b:bs) (x:xs) = let y' = b*x + (1-b)*y in y' : loop y' bs xs
    loop _ _ _ = []

lowpassrc :: [Double] -> [Double] -> [Double]
lowpassrc cs xs = loop 0 0 cs xs
  where
    loop v0 v1 (c:cs) (x:xs) = v1' : loop v0' v1' cs xs
      where
        r = 0.5
        v0' = (1-r*c)*v0 - c*v1 + c*x
        v1' = (1-r*c)*v1 + c*v0'

noise :: (Double, Double) -> [Double]
noise (lo, hi) = randomList (-1.0, 1.0)
  where
    loop m = m >>= \a -> return (a : unsafePerformIO (loop m))
    toList m = unsafePerformIO (loop m)
    randomList = toList . randomRIO
