module Pure.Spinners.DoubleBounce (DoubleBounce(..),defaultDoubleBounce) where

import Pure hiding (delay)

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt hiding (center)

import Data.Proxy
import GHC.TypeLits
import Prelude hiding (or)

data DoubleBounce :: Nat -> Nat -> Nat -> Symbol -> * where
  DoubleBounce 
    :: (KnownNat width, KnownNat height, KnownNat margin, KnownSymbol color) 
    => DoubleBounce width height margin color

instance 
  ( KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Theme (DoubleBounce width height margin color) 
  where
    theme c = do
      let 
        anim = Txt.tail c

        w, h, m :: Txt -> Txt
        w = fi $ natVal @width Proxy 
        h = fi $ natVal @height Proxy 
        m = fi $ natVal @margin Proxy 

        b :: String
        b = symbolVal (Proxy @color)

      atKeyframes anim do
        is (0%) . or is (100%) $ do
          transform =: scale(0)

        is (50%) do 
          transform =: scale(1)

      is c do
        width    =: w px
        height   =: h px
        margin   =* [m px,auto]
        position =: relative

        child (tag Div) do
          width            =: (100%)
          height           =: (100%)
          border-radius    =: (50%)
          background-color =: toTxt b
          opacity          =: 0.6
          position         =: absolute
          top              =: 0
          left             =: 0
          animation        =: anim <<>> 2 s <<>> infinite <<>> easeinout

        lastChild do
          animation-delay =: (-1) s

instance 
  ( KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Pure (DoubleBounce width height margin color) 
  where
    view _ = Div <| Themed @(DoubleBounce width height margin color) |> [ Div, Div ]

defaultDoubleBounce :: View
defaultDoubleBounce = view (DoubleBounce :: DoubleBounce 40 40 40 "#333")

