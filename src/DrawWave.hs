module DrawWave where

import qualified System.Console.Terminal.Size as Terminal

import Data.List

-- | Draw a wave form in the terminal with given size (columns, rows)
drawWave :: (Int, Int) -> [Double] -> IO ()
drawWave (wwidth, wheight) f = do
  putStrLn $ replicate wwidth '-'
  putStr $ unlines $ transpose sig3
  where
    h2 = fromIntegral wheight / 2
    sig1 = take (fromIntegral wwidth) $ f
    sig2 = map (\x -> min (wheight-1) $ floor (h2 - (x * h2))) sig1
    sig3 = map (\x -> take wheight (replicate x ' ' ++ ('*' : repeat ' '))) sig2

-- | Draw a wave form in the terminal using a default size of 200 columns and 56 rows
drawWaveDefault :: [Double] -> IO ()
drawWaveDefault = drawWave defaultTerminalSize

-- | Draw a wave form in the terminal using the current size of the terminal
drawWaveTerminal :: [Double] -> IO ()
drawWaveTerminal f = do
  sz <- getTerminalSize
  drawWave sz f

-- | Default terminal size (200, 56)
defaultTerminalSize :: (Int, Int)
defaultTerminalSize = (200, 56)

-- | get the current size of the terminal (columns, rows)
getTerminalSize :: IO (Int, Int)
getTerminalSize = Terminal.size >>= \mbw -> return $ maybe defaultTerminalSize (\w -> (Terminal.width w, Terminal.height w)) mbw

