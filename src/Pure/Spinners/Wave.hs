module Pure.Spinners.Wave (Wave(..),defaultWave) where

import Pure hiding (delay)

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt hiding (center)

import Data.Proxy
import GHC.TypeLits
import Prelude hiding (or)

data Wave :: Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Symbol -> * where
    Wave :: 
        ( KnownNat count
        , KnownNat duration
        , KnownNat delay
        , KnownNat height
        , KnownNat width
        , KnownNat margin
        , KnownSymbol color
        ) => Wave count duration delay height width margin color


instance 
  ( KnownNat count
  , KnownNat duration
  , KnownNat delay
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Theme (Wave count duration delay width height margin color) 
  where
    theme c = do
      let
        anim = Txt.tail c

        d, p :: Double
        n = fi $ natVal @count Proxy
        d = fi $ natVal @duration Proxy
        p = fi $ natVal @delay Proxy

        w, h, m :: Txt -> Txt
        w = fi $ natVal @width Proxy
        h = fi $ natVal @height Proxy
        m = fi $ natVal @margin Proxy

        b :: String
        b = symbolVal @color Proxy

      atKeyframes anim $ do
        is (0%) . or is (40%) . or is (100%) .> 
          transform =: scaleY(0.4)

        is (20%) .> 
          transform =: scaleY(1)

      void $ is c $ do
        apply $ do
          margin     =* [m px,auto]
          height     =: h px
          width      =: calc(w px * 1.25)
          text-align =: center
          font-size  =: 10px

        child (tag Div) $ do
          apply $ do
            background-color =: toTxt b
            height           =: (100%)
            width            =: 6px
            display          =: inline-block
            animation        =: anim <<>> d <#> ms <<>> infinite <<>> easeinout

          for_ [1..n] $ \r -> 
            nthChild (rtn r) .>
              let t = d + p / (n - 1) * (n - r)
              in animation-delay =: negate t <#> ms 

instance 
  ( KnownNat count
  , KnownNat duration
  , KnownNat delay
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Pure (Wave count duration delay width height margin color) 
  where
    view _ = 
        let c = fi $ natVal @count Proxy 
        in Div <| Themed @(Wave count duration delay width height margin color) |> Prelude.replicate c Div

defaultWave :: View
defaultWave = view (Wave :: Wave 5 1200 400 40 40 40 "#333")
