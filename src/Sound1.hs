{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
module Sound1 where

import Data.Kind (Type)
import Data.Proxy
import Data.Proxy.Mapping
import GHC.TypeLits

import SoundBase
import Rational

-------------------------------------------------------------------------------
class Sound s where
  sound :: Proxy s -> [Double]

class Sound1 s where
  sound1 :: Proxy s -> [Double] -> [Double]

class Sound2 s where
  sound2 :: Proxy s -> [Double] -> [Double] -> [Double]

class Sound3 s where
  sound3 :: Proxy s -> [Double] -> [Double] -> [Double] -> [Double]
-------------------------------------------------------------------------------
instance (KnownNat n, KnownNat d, 1 <= d) => Sound (n :%: d) where
  sound p = constant $ fromRational $ fromRatioProxy p
-------------------------------------------------------------------------------
data Sine s

instance Sound s => Sound (Sine s) where
  sound p = sine $ sound (proxy1of1 p)

instance Sound1 Sine where
  sound1 p = sine
-------------------------------------------------------------------------------
data Phasor s

instance Sound s => Sound (Phasor s) where
  sound p = phasor $ sound (proxy1of1 p)

instance Sound1 Phasor where
  sound1 p = phasor
-------------------------------------------------------------------------------
data Mult s1 s2

instance (Sound s1, Sound s2) => Sound (Mult s1 s2) where
  sound p = mult (sound $ proxy1of2 p) (sound $ proxy2of2 p)

instance Sound s1 => Sound1 (Mult s1) where
  sound1 p = mult (sound $ proxy1of1 p)

instance Sound2 Mult where
  sound2 p = mult
-------------------------------------------------------------------------------
data Add s1 s2

instance (Sound s1, Sound s2) => Sound (Add s1 s2) where
  sound p = add (sound $ proxy1of2 p) (sound $ proxy2of2 p)

instance Sound s1 => Sound1 (Add s1) where
  sound1 p = add (sound $ proxy1of1 p)

instance Sound2 Add where
  sound2 p = add
-------------------------------------------------------------------------------
data Sub s1 s2

instance (Sound s1, Sound s2) => Sound (Sub s1 s2) where
  sound p = sub (sound $ proxy1of2 p) (sound $ proxy2of2 p)

instance Sound s1 => Sound1 (Sub s1) where
  sound1 p = sub (sound $ proxy1of1 p)

instance Sound2 Sub where
  sound2 p = sub
-------------------------------------------------------------------------------
data Neg s

instance Sound s => Sound (Neg s) where
  sound p = neg $ sound (proxy1of1 p)

instance Sound1 Neg where
  sound1 p = neg
-------------------------------------------------------------------------------
data Lowpass s1 s2

instance (Sound s1, Sound s2) => Sound (Lowpass s1 s2) where
  sound p = lowpassrc (sound $ proxy1of2 p) (sound $ proxy2of2 p)

instance Sound s1 => Sound1 (Lowpass s1) where
  sound1 p = lowpassrc (sound $ proxy1of1 p)

instance Sound2 Lowpass where
  sound2 p = lowpassrc
-------------------------------------------------------------------------------
data Chain :: (t1 -> t2 -> Type) -> (t1 -> t4 -> t2) -> t1 -> t4 -> Type

instance (Sound2 f, Sound2 g, Sound c, Sound x) => Sound (Chain f g c x) where
  sound p =
    let
      c = sound (proxy3of4 p)
    in
      sound2 (proxy1of4 p) c (sound2 (proxy2of4 p) c (sound (proxy4of4 p)))

instance (Sound2 f, Sound2 g, Sound c) => Sound1 (Chain f g c) where
  sound1 p x =
    let
      c = sound (proxy3of3 p)
    in
      sound2 (proxy1of3 p) c (sound2 (proxy2of3 p) c x)

instance (Sound2 f, Sound2 g) => Sound2 (Chain f g) where
  sound2 p c x =
    sound2 (proxy1of2 p) c (sound2 (proxy2of2 p) c x)
-------------------------------------------------------------------------------
data Both (p :: t1 -> t2 -> t3) (f :: Type -> t1) (g :: Type -> t2) (c :: Type)

instance (Sound2 p, Sound1 f, Sound1 g, Sound c) => Sound (Both p f g c) where
  sound p =
    let
      c = sound (proxy4of4 p)
    in
      sound2 (proxy1of4 p) (sound1 (proxy2of4 p) c) (sound1 (proxy3of4 p) c)

instance (Sound2 p, Sound1 f, Sound1 g) => Sound1 (Both p f g) where
  sound1 p c =
    sound2 (proxy1of3 p) (sound1 (proxy2of3 p) c) (sound1 (proxy3of3 p) c)
-------------------------------------------------------------------------------
data Flip (t :: Type -> Type -> Type) (x :: Type) (y :: Type)

instance Sound2 t => Sound2 (Flip t) where
  sound2 p x y = sound2 (proxy1of1 p) y x

instance (Sound2 t, Sound x) => Sound1 (Flip t x) where
  sound1 p y = sound2 (proxy1of2 p) y (sound $ proxy2of2 p)

instance (Sound2 t, Sound x, Sound y) => Sound (Flip t x y) where
  sound p = sound2 (proxy1of3 p) (sound $ proxy3of3 p) (sound $ proxy2of3 p)
-------------------------------------------------------------------------------
data S (f :: Type -> Type -> Type) (g :: Type -> Type) (x :: Type)

instance (Sound2 f, Sound1 g, Sound x) => Sound (S f g x) where
  sound p =
    let
      c = sound (proxy3of3 p)
    in
      sound2 (proxy1of3 p) c (sound1 (proxy2of3 p) c)

instance (Sound2 f, Sound1 g) => Sound1 (S f g) where
  sound1 p c =
    sound2 (proxy1of2 p) c (sound1 (proxy2of2 p) c)
-------------------------------------------------------------------------------
data C (f :: Type -> Type) (g :: Type -> Type) :: Type -> Type

instance (Sound1 f, Sound1 g) => Sound1 (C f g) where
  sound1 p x = sound1 (proxy1of2 p) $ sound1 (proxy2of2 p) x
-------------------------------------------------------------------------------
type Freq = 1 :%: 20

-- simple synth
type Synth1 = Mult (1 :%: 2) (Sine (Phasor Freq))
-------------------------------------------------------------------------------
type Harmonic n = (Mult (1 :%: n) (Sine (Phasor (Mult (n :%: 1) Freq))))

type family Saw (n :: Nat) where
  Saw 0 = Harmonic 1
  Saw 1 = Harmonic 1
  Saw n = Add (Harmonic n) (Saw (n-1))

type family Rect (n :: Nat) where
  Rect n = Rect' (n * 2 + 1)

type family Rect' (n :: Nat) where
  Rect' 0 = Harmonic 1
  Rect' 1 = Harmonic 1
  Rect' n = Add (Harmonic n) (Rect' (n-2))

type family LowpassQ (n :: Nat) where
  LowpassQ 0 = Lowpass
  LowpassQ n = Chain (LowpassQ (n-1)) Lowpass
-------------------------------------------------------------------------------
-- sawtooth
type Synth2 = Mult (1 :%: 2) (Saw 10)

-- rectangle
type Synth3 = Mult (1 :%: 2) (Rect 9)

-- lowpass
type Synth4 = Mult (1 :%: 2) (Lowpass (1 :%: 3) (Rect 95))

type Synth5 = Mult (1 :%: 2) (Lowpass LFO (Rect 95))

type Synth6 = Mult (1 :%: 2) (Chain Lowpass Lowpass LFO (Rect 95))

type Synth7 = Mult (1 :%: 2) (Chain (Chain Lowpass Lowpass) Lowpass LFO (Rect 95))

type Synth8 = Mult (1 :%: 2) (LowpassQ 30 LFO2 (Rect 19))

type Synth9 = Mult (1 :%: 2) (S (Flip Sub) (LowpassQ 30 LFO) (Saw 21))
-------------------------------------------------------------------------------
type Harmonic2 n = C (Mult (1 :%: n)) (C Sine (C Phasor (Mult (n :%: 1))))

type family Saw2 (n :: Nat) where
  Saw2 0 = Harmonic2 1
  Saw2 1 = Harmonic2 1
  Saw2 n = Both Add (Harmonic2 n) (Saw2 (n-1))

type family Rect2 (n :: Nat) where
  Rect2 0 = Harmonic2 1
  Rect2 1 = Harmonic2 1
  Rect2 n = Both Add (Harmonic2 n) (Rect2 (n-2))
-------------------------------------------------------------------------------
type LFO = Add (1 :%: 2) (Mult (1 :%: 2) (Sine (Phasor (31415 :%: 300000000))))

type LFO2 = Add (2 :%: 3) (Mult (1 :%: 3) (Sine (Phasor (31415 :%: 500000000))))

type LFO3 = Add (1 :%: 2) (Mult (1 :%: 2) (Sine (Phasor (31415 :%: 700000000))))

type FreqLFO = Mult Freq LFO2
-------------------------------------------------------------------------------
type Synth10 = Mult (1 :%: 2) (Saw2 30 FreqLFO)

type Synth11 = Mult (1 :%: 2) (LowpassQ 5 LFO3 (Saw2 91 FreqLFO))

-- lowpass with lfo cutoff, rect + saw with freq lfo
type Synth12 = Mult (1 :%: 3) (LowpassQ 5 LFO3 (Both Add (Saw2 11) (Rect2 11) FreqLFO))
-------------------------------------------------------------------------------
