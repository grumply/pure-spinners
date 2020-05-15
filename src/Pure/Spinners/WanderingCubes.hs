{-# LANGUAGE OverloadedStrings, GADTs, DataKinds, KindSignatures, TypeApplications, ScopedTypeVariables #-}
module Pure.Spinners.WanderingCubes (WanderingCubes(..),defaultWanderingCubes) where

import Pure.Spinners.Utils

import Pure

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt

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
                d, x, w, h, m :: Int
                d = fromIntegral $ natVal @duration Proxy 
                x = fromIntegral $ natVal @distance Proxy 
                w = fromIntegral $ natVal @width Proxy 
                h = fromIntegral $ natVal @height Proxy 
                m = fromIntegral $ natVal @margin Proxy 

                b :: String
                b = symbolVal @color Proxy 

            keyframes (Txt.tail c) $ do
                is (per 0)    .> trans (rotate(deg 0))
                is (per 25)   .> trans (translateX(pxs x) <<>> rotate(neg (deg 90)) <<>> scale(dec 0.5))
                is (per 50)   .> trans (translateX(pxs x) <<>> translateY(pxs x) <<>> rotate(neg(deg 179)))
                is (per 50.1) .> trans (translateX(pxs x) <<>> translateY(pxs x) <<>> rotate(neg(deg 180)))
                is (per 75)   .> trans (translateX(zero) <<>> translateY(pxs x) <<>> rotate(neg(deg 270)) <<>> scale(dec 0.5))
                is (per 100)  .> trans (rotate(neg(deg 360)))

            void $ is c $ do
                apply $ do
                    margin =: pxs m <<>> auto
                    height =: pxs h
                    width =: pxs w
                    position =: relative

                child (tag Div) $ do
                    apply $ do
                        backgroundColor =: toTxt b
                        width =: pxs 10
                        height =: pxs 10
                        position =: absolute
                        top =: zero
                        left =: zero
                        anim $ Txt.tail c <<>> ms d <<>> "ease-in-out" <<>> neg (ms d) <<>> "infinite both"

                    is (nth 2) .>
                        "animation-delay" =: neg (ms (d `div` 2))

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

