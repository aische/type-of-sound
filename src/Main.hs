module Main where

import Data.Proxy

import qualified Sound1 as Sound1
import qualified Sound2 as Sound2
import DrawWave
import SaveWave

main :: IO ()
main = do
  example1
  example2
  example3
  example4
  out1
  out2
  out3
  out4
  out1b

example1 = drawWaveTerminal $ Sound1.sound (Proxy :: Proxy Sound1.Synth1)
example2 = drawWaveTerminal $ Sound1.sound (Proxy :: Proxy Sound1.Synth2)
example3 = drawWaveTerminal $ Sound1.sound (Proxy :: Proxy Sound1.Synth3)
example4 = drawWaveTerminal $ Sound1.sound (Proxy :: Proxy Sound1.Synth12)

out1 = saveWaveMono "out1_1.wav" 10 $ Sound1.sound (Proxy :: Proxy Sound1.Synth1)
out2 = saveWaveMono "out1_2.wav" 10 $ Sound1.sound (Proxy :: Proxy Sound1.Synth2)
out3 = saveWaveMono "out1_2.wav" 10 $ Sound1.sound (Proxy :: Proxy Sound1.Synth2)
out4 = saveWaveMono "out1_4.wav" 10 $ Sound1.sound (Proxy :: Proxy Sound1.Synth12)

out1b = saveWaveMono "out2_1.wav" 10 $ Sound2.getCurrentSignal $ Sound2.sound (Proxy :: Proxy Sound2.Synth1) Sound2.emptySignals

{-
:set -XTypeOperators
:set -XDataKinds
:set -freduction-depth=0
drawWaveTerminal $ Sound1.sound (Proxy :: Proxy (Sound1.Mult (1 :%: 2) (Sound1.Saw 10)))
-}