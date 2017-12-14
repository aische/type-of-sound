# Type of sound

In this experiment I'm trying to define synthesizers only as types (without values). There are two approaches: In the first approach, the synthesizer types look like expressions. In the second approach, the synthesizer types look more like imperative programs that manipulate a stack (resp. a list zipper) with signals.

## Sound1

The basic idea of this project is to have type classes like this:

	class Sound s where
  	  sound :: Proxy s -> [Double]

	class Sound1 s where
	  sound1 :: Proxy s -> [Double] -> [Double]

	class Sound2 s where
	  sound2 :: Proxy s -> [Double] -> [Double] -> [Double]

and a lot of types that instanciate some of these type classes:

	data Sine s

	instance Sound s => Sound (Sine s) where
	  sound p = map sin $ sound (proxy1a p)

	instance Sound1 Sine where
	  sound1 p = map sin

 A synthesizer can then be defined as a type:

	type Freq = 1 :%: 20

	type Harmonic n = (Mult (1 :%: n) (Sine (Phasor (Mult (n :%: 1) Freq))))

	type family Saw (n :: Nat) where
	  Saw 0 = Harmonic 1
	  Saw 1 = Harmonic 1
	  Saw n = Add (Harmonic n) (Saw (n-1))

	type Example = Mult (1 :%: 2) (Saw 11)

	saveWaveMono "out.wav" 10 $ sound (Proxy :: Proxy Example)
	
In this example a synthesizer is defined using the "Saw" type family. It creates a sawtooth wave by adding a the harmonics with amplitudes reciprocal to their frequency. The expanded type looks like this:

	ghci> :kind! Example
	
    Example :: *
    = Mult
        (1 :%: 2)
        (Add
           (Harmonic 10)
           (Add
              (Harmonic 9)
              (Add
                 (Harmonic 8)
                 (Add
                    (Harmonic 7)
                    (Add
                       (Harmonic 6)
                       (Add
                          (Harmonic 5)
                          (Add
                             (Harmonic 4)
                             (Add
                                (Harmonic 3)
                                (Add (Harmonic 2) 
                                     (Harmonic 1))))))))))
                                     
The sawtooth synth uses the same frequency type for all harmonics. The frequency signal will be computed for each harmonic. This is not desired, especially if the frequency signal is the result of an expensive computation. A second version of the sawtooth generator is shown below. Here the frequency signal is shared by using a combinator "Both".

    type Harmonic2 n = C (Mult (1 :%: n)) (C Sine (C Phasor (Mult (n :%: 1))))

    type family Saw2 (n :: Nat) where
      Saw2 0 = Harmonic2 1
      Saw2 1 = Harmonic2 1
      Saw2 n = Both Add (Harmonic2 n) (Saw2 (n-1))

    data Both (p :: t1 -> t2 -> t3) (f :: Type -> t1) (g :: Type -> t2) (c :: Type)

    instance (Sound2 p, Sound1 f, Sound1 g, Sound c) => Sound (Both p f g c) where
      sound p =
        let
          c = sound (proxy4of4 p)
        in
          sound2 (proxy1of4 p) (sound1 (proxy2of4 p) c) (sound1 (proxy3of4 p) c)

Using combinators to share signals becomes tedious because different combinators for different arities are needed (at least this is how I interpret the error messages I got). So if there is more than one signal that should be shared, a different "Both" combinator is needed. I did not explore that further and tried a second approach:


## Sound2

The second approach uses a list zipper and operations which transformed it:

	data Signals (l :: Nat) (r :: Nat) = Signals [[Double]] [[Double]]

The building blocks of the synthesizer types are not used to build expressions like in the first approach. They are commands that operate on the two stacks of the list zipper. The sound class looks like this:

	class Sound t s where
	  type SoundType t s
	  sound :: Proxy t -> s -> SoundType t s

and the Sound instance for Add:

	data Add

	instance (2 <= ar) => Sound Add (Signals al ar) where
	  type SoundType Add (Signals al ar) = (Signals al (ar - 1))
	  sound _ (Signals al (x : y : ar)) = Signals al (add x y : ar)

This is not as beautiful as using expression-like types, but it is more powerful. Signals can be shared without limitations and without using complicated combinators. On the other hand, one has to know the state of the stack all the time...

    type Harmonic n = Do [ n :%: 1, Mult, Phasor, Sine, 1 :%: n, Mult ]

    type family Saw (n :: Nat) where
      Saw 0 = Harmonic 1
      Saw 1 = Harmonic 1
      Saw n = Do [ Dup, MoveR, Harmonic n, MoveL, Saw (n-1), Add ]

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

