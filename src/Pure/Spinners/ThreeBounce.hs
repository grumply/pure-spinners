module Pure.Spinners.ThreeBounce (ThreeBounce(..),defaultThreeBounce) where

import Pure hiding (delay)

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt hiding (center)

import Data.Proxy
import GHC.TypeLits
import Prelude hiding (or)

data ThreeBounce :: Nat -> Nat -> Nat -> Nat -> Nat -> Symbol -> * where
  ThreeBounce
    :: ( KnownNat duration
       , KnownNat delay
       , KnownNat width
       , KnownNat height
       , KnownNat margin
       , KnownSymbol color
       ) => ThreeBounce duration delay width height margin color

instance
  ( KnownNat duration
  , KnownNat delay
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Theme (ThreeBounce duration delay width height margin color)
  where
    theme c = do
      let
        anim = Txt.tail c

        d, p :: Double
        d = fi $ natVal @duration Proxy 
        p = fi $ natVal @delay Proxy 

        w, h, m :: Txt -> Txt
        w = fi $ natVal @width Proxy 
        h = fi $ natVal @height Proxy 
        m = fi $ natVal @margin Proxy 

        b :: String
        b = symbolVal @color Proxy

      atKeyframes anim do
        is (0%) . or is (80%) . or is (100%) $ do
          transform =: scale(0)

        is (40%) do
          transform =: scale(1)
    
      is c do
        margin     =* [m px,auto]
        width      =: calc(w px * 2)
        text-align =: center

        child (tag Div) do
          width            =: calc(w px / 2)
          height           =: calc(h px / 2)
          background-color =: toTxt b
          border-radius    =: (100%)
          display          =: inline-block
          animation        =: anim <<>> d <#> ms <<>> easeinout <<>> 0 s <<>> infinite <<>> both

          nthChild 1 do 
            animation-delay =: negate p <#> ms

          nthChild 2 do 
            animation-delay =: negate (p / 2) <#> ms

instance
  ( KnownNat duration
  , KnownNat delay
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Pure (ThreeBounce duration delay width height margin color)
  where
    view _ = 
      Div <| Themed @(ThreeBounce duration delay width height margin color) |>
        [ Div, Div, Div ]

defaultThreeBounce :: View
defaultThreeBounce = view (ThreeBounce :: ThreeBounce 1400 320 40 40 40 "#333")
