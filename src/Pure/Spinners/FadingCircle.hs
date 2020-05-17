module Pure.Spinners.FadingCircle (FadingCircle(..),defaultFadingCircle) where

import Pure hiding (delay)

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt hiding (center)

import Data.Proxy
import GHC.TypeLits
import Prelude hiding (or)

data FadingCircle :: Nat -> Nat -> Nat -> Nat -> Nat -> Symbol -> * where
  FadingCircle
    :: ( KnownNat count
       , KnownNat duration
       , KnownNat width
       , KnownNat height
       , KnownNat margin
       , KnownSymbol color
       ) => FadingCircle count duration width height margin color


instance 
  ( KnownNat count
  , KnownNat duration
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Theme (FadingCircle count duration width height margin color)
  where
    theme c = do
      let
        anim = Txt.tail c

        n, d :: Double
        n = fi $ natVal @count Proxy 
        d = fi $ natVal @duration Proxy 

        w, h, m :: Txt -> Txt
        w = fi $ natVal @width Proxy 
        h = fi $ natVal @height Proxy 
        m = fi $ natVal @margin Proxy 

        b :: String
        b = symbolVal @color Proxy 

      atKeyframes anim $ do
        is (0%) . or is (39%) . or is (100%) .> 
          opacity =: 0

        is (40%) .> 
          opacity =: 1

      void $ is c $ do
        apply $ do
          width    =: w px
          height   =: h px
          margin   =* [m px,auto]
          position =: relative

        child (tag Div) $ do
          apply $ do
            width    =: (100%)
            height   =: (100%)
            position =: absolute
            left     =: 0
            top      =: 0

          is before .> do
            content          =: emptyQuotes
            display          =: block
            margin           =* [zero,auto]
            width            =: (15%)
            height           =: (15%)
            background-color =: toTxt b
            border-radius    =: (100%)
            animation        =: anim <<>> d <#> ms <<>> infinite <<>> easeinout <<>> both

          for_ [2..n] $ \i -> do
            nthChild (rtn i) .>
              let d = 360 / n * (i - 1) 
              in transform =: rotate(d <#> deg)

            nthChild (rtn i) . is before .> 
              let t = d + d / n * (n - i)
              in animation-delay =: negate t <#> s

instance
  ( KnownNat count
  , KnownNat duration
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Pure (FadingCircle count duration width height margin color)
  where
    view _ = 
      let c = fi $ natVal @count Proxy 
      in Div <| Themed @(FadingCircle count duration width height margin color) |> 
           Prelude.replicate c Div

defaultFadingCircle :: View
defaultFadingCircle = view (FadingCircle :: FadingCircle 12 1200 40 40 40 "#333")
