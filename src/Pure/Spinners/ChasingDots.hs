module Pure.Spinners.ChasingDots (ChasingDots(..),defaultChasingDots) where

import Pure hiding (delay)

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt hiding (center)

import Data.Proxy
import GHC.TypeLits
import Prelude hiding (or)

data ChasingDots :: Nat -> Nat -> Nat -> Nat -> Symbol -> * where
  ChasingDots
    :: ( KnownNat duration
       , KnownNat width
       , KnownNat height
       , KnownNat margin
       , KnownSymbol color
       ) => ChasingDots duration width height margin color

instance
  ( KnownNat duration
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Theme (ChasingDots duration width height margin color) 
  where
    theme c = do
      let 
        rota   = Txt.tail c <> "_rotate"
        bounce = Txt.tail c <> "_bounce"

        d :: Double
        d = fi $ natVal @duration Proxy

        w, h, m :: Txt -> Txt
        w = fi $ natVal @width Proxy 
        h = fi $ natVal @height Proxy 
        m = fi $ natVal @margin Proxy 

        b :: String
        b = symbolVal @color Proxy 

      atKeyframes rota do
        is (100%) do 
          transform =: rotate(360 deg)

      atKeyframes bounce do
        is (0%) . or is (100%) $ do
            transform =: scale(0)

        is (50%) do 
          transform =: scale(1)

      is c do
        margin     =* [m px,auto]
        height     =: h px
        width      =: w px
        position   =: relative
        text-align =: center
        animation  =: rota <<>> d <#> ms <<>> infinite <<>> linear

        child (tag Div) do
          width            =: (60%)
          height           =: (60%)
          display          =: inline-block
          position         =: absolute
          top              =: 0
          background-color =: toTxt b
          border-radius    =: (100%)
          animation        =: bounce <<>> d <#> ms <<>> infinite <<>> easeinout

        child (tag Div) do 
          pseudo "last-child" do
            top             =: auto
            bottom          =: 0
            animation-delay =: negate (d / 2) <#> ms


instance
  ( KnownNat duration
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Pure (ChasingDots duration width height margin color) 
  where
    view _ = 
      Div <| Themed @(ChasingDots duration width height margin color) |> 
        [ Div, Div ]

defaultChasingDots :: View
defaultChasingDots = view (ChasingDots :: ChasingDots 2000 40 40 40 "#333")
