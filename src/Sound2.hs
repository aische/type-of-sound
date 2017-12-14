{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
module Sound2 where

import Data.Kind (Type)
import Data.Proxy
import Data.Proxy.Mapping
import GHC.TypeLits

import SoundBase
import Rational
-------------------------------------------------------------------------------
data Signals (l :: Nat) (r :: Nat) = Signals [[Double]] [[Double]]

emptySignals :: Signals 0 0
emptySignals = Signals [] []

fromSignals :: Signals m n -> [[Double]]
fromSignals (Signals l r) = loop l r
  where
    loop [] r = r
    loop (l:ls) r = loop ls (l:r)

getCurrentSignal :: (1 <= n) => Signals m n -> [Double]
getCurrentSignal (Signals l (x:r)) = x
-------------------------------------------------------------------------------
class Sound t s where
  type SoundType t s
  sound :: Proxy t -> s -> SoundType t s
-------------------------------------------------------------------------------
instance (KnownNat n, KnownNat d, 1 <= d) => Sound (n :%: d) (Signals al ar) where
  type SoundType (n :%: d) (Signals al ar) = Signals al (ar+1)
  sound p (Signals al ar) = Signals al (c : ar)
    where
      c = repeat $ fromRational $ fromRatioProxy p
-------------------------------------------------------------------------------
data MoveR

instance (1 <= ar) => Sound MoveR (Signals al ar) where
  type SoundType MoveR (Signals al ar) = Signals (al + 1) (ar - 1)
  sound _ (Signals al (a : ar)) = Signals (a : al) ar
-------------------------------------------------------------------------------
data MoveL

instance (1 <= al) => Sound MoveL (Signals al ar) where
  type SoundType MoveL (Signals al ar) = Signals (al - 1) (ar + 1)
  sound _ (Signals (a : al) ar) = Signals al (a : ar)
-------------------------------------------------------------------------------
data Swap

instance (1 <= al, 1 <= ar) => Sound Swap (Signals al ar) where
  type SoundType Swap (Signals al ar) = (Signals al ar)
  sound _ (Signals (a : al) (b : ar)) = Signals (b : al) (a : ar)
-------------------------------------------------------------------------------
data Dup

instance (1 <= ar) => Sound Dup (Signals al ar) where
  type SoundType Dup (Signals al ar) = (Signals al (ar + 1))
  sound _ (Signals al (a : ar)) = Signals al (a : (a : ar))
-------------------------------------------------------------------------------
data Delete

instance (1 <= ar) => Sound Delete (Signals al ar) where
  type SoundType Delete (Signals al ar) = (Signals al (ar - 1))
  sound _ (Signals al (a : ar)) = Signals al ar
-------------------------------------------------------------------------------
data Add

instance (2 <= ar) => Sound Add (Signals al ar) where
  type SoundType Add (Signals al ar) = (Signals al (ar - 1))
  sound _ (Signals al (x : y : ar)) = Signals al (add x y : ar)
-------------------------------------------------------------------------------
data Sub

instance (2 <= ar) => Sound Sub (Signals al ar) where
  type SoundType Sub (Signals al ar) = (Signals al (ar - 1))
  sound _ (Signals al (x : y : ar)) = Signals al (sub x y : ar)
-------------------------------------------------------------------------------
data Mult

instance (2 <= ar) => Sound Mult (Signals al ar) where
  type SoundType Mult (Signals al ar) = (Signals al (ar - 1))
  sound _ (Signals al (x : y : ar)) = Signals al (mult x y : ar)
-------------------------------------------------------------------------------
data LowpassQ

instance (2 <= ar) => Sound LowpassQ (Signals al ar) where
  type SoundType LowpassQ (Signals al ar) = (Signals al (ar - 1))
  sound _ (Signals al (x : y : ar)) = Signals al (lowpassrc x y : ar)
-------------------------------------------------------------------------------
data Sine

instance (1 <= ar) => Sound Sine (Signals al ar) where
  type SoundType Sine (Signals al ar) = Signals al ar
  sound _ (Signals al (phase : ar)) = Signals al (sine phase : ar)
-------------------------------------------------------------------------------
data Phasor

instance (1 <= ar) => Sound Phasor (Signals al ar) where
  type SoundType Phasor (Signals al ar) = Signals al ar
  sound _ (Signals al (freq : ar)) = Signals al (phasor freq : ar)
-------------------------------------------------------------------------------
data s1 :>>> s3

instance (Sound s1 (Signals al ar), Sound s2 (Signals ol or), SoundType s1 (Signals al ar) ~ (Signals ol or)) => Sound (s1 :>>> s2) (Signals al ar) where
  type SoundType (s1 :>>> s2) (Signals al ar) = SoundType s2 (SoundType s1 (Signals al ar))
  sound p lz = sound (proxy2of2 p) $ sound (proxy1of2 p) lz
-------------------------------------------------------------------------------
data Do (s :: [Type])

instance Sound (Do '[]) (Signals al ar) where
  type SoundType (Do '[]) (Signals al ar) = Signals al ar
  sound p s = s

instance (Sound t (Signals al ar), Sound (Do r) (Signals ol or), SoundType t (Signals al ar) ~ (Signals ol or)) => Sound (Do (t ': r)) (Signals al ar) where
  type SoundType (Do (t ': r)) (Signals al ar) = SoundType (Do r) (SoundType t (Signals al ar))
  sound p s = sound (proxyApply (proxy0of1 p) (proxyTail $ proxy1of1 p)) $ sound (proxyHead $ proxy1of1 p) s
-------------------------------------------------------------------------------
type Harmonic n = Do [ n :%: 1, Mult, Phasor, Sine, 1 :%: n, Mult ]

type family Saw (n :: Nat) where
  Saw 0 = Harmonic 1
  Saw 1 = Harmonic 1
  Saw n = Do [ Dup, MoveR, Harmonic n, MoveL, Saw (n-1), Add ]

type family Rect' (n :: Nat) :: k where
  Rect' 0 = Harmonic 1
  Rect' 1 = Harmonic 1
  Rect' n = Do [ Dup, MoveR, Harmonic n, MoveL, Rect' (n-2), Add ]

type family Rect (n :: Nat) where
  Rect n = Rect' (n * 2 + 1)

type family LowpassQN (n :: Nat) where
  LowpassQN 0 = Do '[Delete]
  LowpassQN n = Do [ Dup, MoveR, LowpassQ, MoveL, LowpassQN (n-1) ]
-------------------------------------------------------------------------------
type family DupN n where
  DupN 0 = Do '[]
  DupN 1 = Dup
  DupN n = DupN' n n :>>> MoveLeftN n

type family DupN' c n where
  DupN' 0 n = Do '[]
  DupN' c n = Do [ Dup, MoveR, MoveR, Swap, SwapRightN (n-2), MoveLeftN (n-1), DupN' (c-1) n ]

type family SwapRightN n where
  SwapRightN 0 = Do '[]
  SwapRightN n = MoveR :>>> Swap :>>> SwapRightN (n-1)

type family MoveLeftN n where
  MoveLeftN 0 = Do '[]
  MoveLeftN n = MoveL :>>> MoveLeftN (n-1)
-------------------------------------------------------------------------------
type MixLFO = Do [ 1 :%: 50000, Phasor, Sine, 1 :%: 2, Mult, 1 :%: 2, Add ]

type FreqLFO = Do [ 1 :%: 10000, Phasor, Sine, 1 :%: 3, Mult, 2 :%: 3, Add ]

type FilterLFO = Do [ 1 :%: 3000, Phasor, Sine, 1 :%: 2, Mult, 1 :%: 2, Add ]

-- |
--
-- @
--  saveWaveMono "out.wav" 10 $ getCurrentSignal $ sound (Proxy :: Proxy Synth1) emptySignals
-- @
--
type Synth1 =
  Do
    [ FilterLFO    -- push filter cutoff lfo
    , MixLFO       -- push lfo for mixing sawtooth and rect
    , FreqLFO      -- push frequency modulation lfo
    , 1 :%: 60     -- push frequency
    , Mult         -- multiply frequency with frequency modulation lfo
    , DupN 2       -- duplicate frequency and mix-lfo
    , Saw 10       -- create sawtooth from frequency
    , MoveR        -- move sawtooth to the left stack
    , 1 :%: 1      -- push 1
    , Sub          -- subtract mix-lfo from 1
    , MoveL        -- get sawtooth back on right stack
    , Mult         -- multiply sawtooth with (inverted) mix-lfo
    , MoveR        -- move sawtooth to the left stack
    , Rect 10      -- create rectangle wave
    , Mult         -- multiply rectangle with mix-lfo
    , MoveL        -- get sawtooth back on right stack
    , Add          -- add sawtooth and rectangle waves
    , MoveR        --
    , Swap         -- swap wave and filter-lfo
    , MoveL        --
    , LowpassQN 5  -- apply filter to wave
    , 1 :%: 2      -- push 1/2
    , Mult         -- multiply wave with 1/2
    ]
