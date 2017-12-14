module SaveWave where

import WavFile

-- | saveWavMono filename seconds list
--
--   Save a waveform (list of Doubles) to a mono WAV file (16 bit, 44100 kHz).
saveWaveMono :: FilePath -> Int -> [Double] -> IO ()
saveWaveMono fp seconds =
  writeWavFileMono fp .
  map wavDoubleToInt16 .
  take (seconds*44100)

