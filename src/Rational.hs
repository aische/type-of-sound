{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
module Rational where

import GHC.TypeLits
import Data.Proxy
import Data.Proxy.Mapping
import Data.Ratio

data (n :: Nat) :%: (d :: Nat)

fromRatioProxy :: (KnownNat n, KnownNat d, 1 <= d) => Proxy (n :%: d) -> Ratio Integer
fromRatioProxy p = (natVal $ proxy1of2 p) % (natVal $ proxy2of2 p)

