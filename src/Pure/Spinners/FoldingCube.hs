module Pure.Spinners.FoldingCube (FoldingCube(..),defaultFoldingCube) where

import Pure hiding (delay)

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt hiding (center)

import Data.Proxy
import GHC.TypeLits
import Prelude hiding (or)

data FoldingCube :: Nat -> Nat -> Nat -> Nat -> Nat -> Symbol -> * where
  FoldingCube
    :: ( KnownNat duration
       , KnownNat delay
       , KnownNat width
       , KnownNat height
       , KnownNat margin
       , KnownSymbol color
       ) => FoldingCube duration delay width height margin color

instance 
  ( KnownNat duration
  , KnownNat delay
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Theme (FoldingCube duration delay width height margin color) 
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
        is (0%) . or is (10%) $ do
          transform =: persp(140px) <<>> rotX((-180)deg)
          opacity   =: 0

        is (25%) . or is (75%) $ do
          transform =: persp(140px) <<>> rotX(0 deg)
          opacity   =: 1

        is (90%) . or is (100%) $ do
          transform =: persp(140px) <<>> rotY(180 deg)
          opacity   =: 0
        
      is c do
        margin    =* [m px,auto]
        width     =: w px
        height    =: h px
        position  =: relative
        transform =: rotateZ(45 deg)

        child (tag Div) do
          float     =: left
          width     =: (50%)
          height    =: (50%)
          position  =: relative
          transform =: scale(1.1)
            
          before do
            content          =: emptyQuotes
            position         =: absolute
            top              =: 0
            left             =: 0
            width            =: (100%)
            height           =: (100%)
            background-color =: toTxt b
            animation        =: anim <<>> d <#> s <<>> infinite <<>> linear <<>> both
            transform-origin =* [(100%),(100%)]

          for_ [(2,1),(3,3),(4,2)] $ \(n,i) -> do
            nthChild (rtn n) do
              transform =: scale(1.1) <<>> rotateZ(90 * i <#> deg)

              before do
                animation-delay =: p / 4 * i <#> ms

instance 
  ( KnownNat duration
  , KnownNat delay
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Pure (FoldingCube duration delay width height margin color) 
  where
    view _ = Div <| Themed @(FoldingCube duration delay width height margin color) |> [ Div, Div, Div, Div ]

defaultFoldingCube :: View
defaultFoldingCube = view (FoldingCube :: FoldingCube 2400 1200 40 40 40 "#333")