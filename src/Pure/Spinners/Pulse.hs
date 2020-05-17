module Pure.Spinners.Pulse (Pulse(..),defaultPulse) where

import Pure hiding (delay)

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt hiding (center)

import Data.Proxy
import GHC.TypeLits
import Prelude

data Pulse :: Nat -> Nat -> Nat -> Nat -> Symbol -> * where
  Pulse
    :: ( KnownNat duration
       , KnownNat width
       , KnownNat height
       , KnownNat margin
       , KnownSymbol color
       ) => Pulse duration width height margin color

instance
  ( KnownNat duration
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Theme (Pulse duration width height margin color)
  where
    theme c = do
      let
        anim = Txt.tail c

        d :: Double
        d = fi $ natVal @duration Proxy 

        w, h, m :: Txt -> Txt
        w = fi $ natVal @width Proxy 
        h = fi $ natVal @height Proxy 
        m = fi $ natVal @margin Proxy 

        b :: String
        b = symbolVal @color Proxy 

      atKeyframes anim $ do
        is (0%) .> 
          transform =: scale(0)

        is (100%) .> do
          transform =: scale(1)
          opacity   =: 0

      void $ is c .> do
        width            =: w px
        height           =: h px
        margin           =* [m px,auto]
        background-color =: toTxt b
        border-radius    =: (100%)
        animation        =: anim <<>> d <#> ms <<>> infinite <<>> easeinout

instance
  ( KnownNat duration
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Pure (Pulse duration width height margin color)
  where
    view _ = Div <| Themed @(Pulse duration width height margin color)

defaultPulse :: View
defaultPulse = view (Pulse :: Pulse 1000 40 40 40 "#333")
