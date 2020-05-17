module Pure.Spinners.RotatingPlane (RotatingPlane(..),defaultRotatingPlane) where

import Pure hiding (delay)

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt hiding (center)

import Data.Proxy
import GHC.TypeLits
import Prelude hiding (or)

data RotatingPlane :: Nat -> Nat -> Nat -> Symbol -> * where
    RotatingPlane 
        :: (KnownNat width, KnownNat height, KnownNat margin, KnownSymbol color) 
        => RotatingPlane width height margin color

instance 
  ( KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Theme (RotatingPlane width height margin color) 
  where
    theme c = do
        let 
            anim = Txt.tail c

            w, h, m :: Txt -> Txt
            w = fi $ natVal @width Proxy 
            h = fi $ natVal @height Proxy 
            m = fi $ natVal @margin Proxy 

            b :: String
            b = symbolVal @color Proxy 

        atKeyframes anim $ do
            let trans x y = 
                    let p = persp(120px)
                        rx = rotX(x deg)
                        ry = rotY(y deg)
                    in transform =: p <<>> rx <<>> ry
            is (0%)   .> trans 0 0 
            is (25%)  .> trans (-180.1) 0
            is (50%)  .> trans (-180) (-179.9)
            is (75%)  .> trans 0 (-179.9)
            is (100%) .> trans 0 0

        void $ is c .> do
            width            =: w px
            height           =: h px
            background-color =: toTxt b
            margin           =* [m px,auto]
            animation        =: anim <<>> 4s <<>> infinite <<>> easeinout

instance 
    ( KnownNat width
    , KnownNat height
    , KnownNat margin
    , KnownSymbol color
    ) => Pure (RotatingPlane width height margin color) 
    where
        view _ = Div <| Themed @(RotatingPlane width height margin color)

defaultRotatingPlane :: View
defaultRotatingPlane = view (RotatingPlane :: RotatingPlane 40 40 40 "#333")

