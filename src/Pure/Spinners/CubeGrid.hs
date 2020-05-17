module Pure.Spinners.CubeGrid (CubeGrid(..),defaultCubeGrid) where

import Pure hiding (delay)

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt hiding (center)

import Data.Proxy
import GHC.TypeLits
import Prelude hiding (or)

data CubeGrid :: Nat -> Nat -> Nat -> Nat -> Nat -> Symbol -> * where
  CubeGrid
    :: ( KnownNat duration
       , KnownNat delay
       , KnownNat width
       , KnownNat height
       , KnownNat margin
       , KnownSymbol color
       ) => CubeGrid duration delay width height margin color

instance 
  ( KnownNat duration
  , KnownNat delay
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Theme (CubeGrid duration delay width height margin color)
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

      atKeyframes anim $ do
        is (0%) . or is (70%) . or is (100%) .> 
          transform =: scale3d(1,1,1)

        is (35%) .> 
          transform =: scale3d(0,0,1)

      void $ is c $ do
        apply $ do
          width  =: w px
          height =: h px
          margin =* [m px,auto]

        child (tag Div) $ do
          apply $ do
            width            =: (33.33%)
            height           =: (33.33%)
            background-color =: toTxt b
            float            =: left
            animation        =: anim <<>> d <#> ms <<>> infinite <<>> easeinout

          for_ (Prelude.zip [1..9] [0.5,0.75,1,0.25,0.5,0.75,0,0.25,0.50]) $ \(n,d) -> 
            nthChild n .> 
              animation-delay =: " " <<>> p * d <#> ms

instance 
  ( KnownNat duration
  , KnownNat delay
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Pure (CubeGrid duration delay width height margin color)
  where
    view _ = Div <| Themed @(CubeGrid duration delay width height margin color) |> Prelude.replicate 9 Div

defaultCubeGrid :: View
defaultCubeGrid = view (CubeGrid :: CubeGrid 1300 400 40 40 40 "#333")