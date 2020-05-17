module Pure.Spinners.Circle (Circle(..),defaultCircle) where

import Pure hiding (delay)

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt hiding (center)

import Data.Proxy
import GHC.TypeLits
import Prelude hiding (or)

data Circle :: Nat -> Nat -> Nat -> Nat -> Nat -> Symbol -> * where
  Circle
    :: ( KnownNat count
       , KnownNat duration
       , KnownNat width
       , KnownNat height
       , KnownNat margin
       , KnownSymbol color
       ) => Circle count duration width height margin color

instance 
  ( KnownNat count
  , KnownNat duration
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Theme (Circle count duration width height margin color)
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
          is (0%) . or is (80%) . or is (100%) .> 
            transform =: scale(0)

          is (40%) .> 
            transform =: scale(1)

      void $ is c $ do
        apply $ do
          margin   =* [m px,auto]
          width    =: w px
          height   =: h px
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
            margin           =* [0,auto]
            width            =: (15%)
            height           =: (15%)
            background-color =: toTxt b
            border-radius    =: (100%)
            animation        =: anim <<>> d <#> ms <<>> infinite <<>> easeinout <<>> both

          for_ [2..n] $ \x -> do
            nthChild (rtn x) .>
              transform =: rotate(360 / n * (x - 1) <#> deg)

            nthChild (rtn x) . is before .> 
                let t = d + d / n * (n - x)
                in animation-delay =: negate t <#> ms

instance
  ( KnownNat count
  , KnownNat duration
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Pure (Circle count duration width height margin color)
  where
    view _ = 
      let c = fi $ natVal @count Proxy
      in Div <| Themed @(Circle count duration width height margin color) |> Prelude.replicate c Div

defaultCircle :: View
defaultCircle = view (Circle :: Circle 12 1200 40 40 40 "#333")
