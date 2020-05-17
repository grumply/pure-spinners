module Pure.Spinners.WanderingCubes (WanderingCubes(..),defaultWanderingCubes) where

import Pure hiding (delay)

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt hiding (center)

import Data.Proxy
import GHC.TypeLits
import Prelude

data WanderingCubes :: Nat -> Nat -> Nat -> Nat -> Nat -> Symbol -> * where
  WanderingCubes
    :: ( KnownNat duration
       , KnownNat distance
       , KnownNat width
       , KnownNat height
       , KnownNat margin
       , KnownSymbol color
       ) => WanderingCubes duration distance width height margin color

instance 
    ( KnownNat duration
    , KnownNat distance
    , KnownNat width
    , KnownNat height
    , KnownNat margin
    , KnownSymbol color
    ) => Theme (WanderingCubes duration distance width height margin color) 
    where
        theme c = do
            let
                anim = Txt.tail c

                d :: Double
                d = fi $ natVal @duration Proxy 

                x, w, h, m :: Txt -> Txt
                x = fi $ natVal @distance Proxy 
                w = fi $ natVal @width Proxy 
                h = fi $ natVal @height Proxy 
                m = fi $ natVal @margin Proxy 

                b :: String
                b = symbolVal @color Proxy 

            atKeyframes anim $ do
                is (0%) .> 
                    transform =: rot(0 deg)
                is (25%) .> 
                    transform =: translateX(x px) <<>> rot((-90)deg) <<>> scale(0.5)
                is (50%) .> 
                    transform =: translateX(x px) <<>> translateY(x px) <<>> rot((-179)deg)
                is (50.1%) .> 
                    transform =: translateX(x px) <<>> translateY(x px) <<>> rot((-180)deg)
                is (75%) .> 
                    transform =: translateX(0px) <<>> translateY(x px) <<>> rot((-270)deg) <<>> scale(0.5)
                is (100%) .> 
                    transform =: rot((-360)deg)

            void $ is c $ do
                apply $ do
                    margin   =* [m px,auto]
                    height   =: h px
                    width    =: w px
                    position =: relative

                child (tag Div) $ do
                    apply $ do
                        background-color =: toTxt b
                        width            =: 10 px
                        height           =: 10 px
                        position         =: absolute
                        top              =: 0
                        left             =: 0
                        animation        =: anim <<>> d <#> ms <<>> easeinout <<>> negate d <#> ms <<>> infinite <<>> both

                    nthChild 2 .>
                        animation-delay =: negate (d / 2) <#> ms

instance 
    ( KnownNat duration
    , KnownNat distance
    , KnownNat width
    , KnownNat height
    , KnownNat margin
    , KnownSymbol color
    ) => Pure (WanderingCubes distance duration width height margin color) 
    where
        view _ = 
            Div <| Themed @(WanderingCubes distance duration width height margin color) |> [ Div, Div ]

defaultWanderingCubes :: View
defaultWanderingCubes = view (WanderingCubes :: WanderingCubes 1800 30 40 40 40 "#333")

