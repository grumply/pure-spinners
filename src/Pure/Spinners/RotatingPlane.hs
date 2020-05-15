{-# LANGUAGE OverloadedStrings, GADTs, DataKinds, KindSignatures, TypeApplications, ScopedTypeVariables #-}
module Pure.Spinners.RotatingPlane (RotatingPlane(..),defaultRotatingPlane) where

import Pure.Spinners.Utils

import Pure

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt

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
            w, h, m :: Int
            w = fromIntegral $ natVal @width Proxy 
            h = fromIntegral $ natVal @height Proxy 
            m = fromIntegral $ natVal @margin Proxy 

            b :: String
            b = symbolVal @color Proxy 

        keyframes (Txt.tail c) $ do
            is (per 0)   .> trans (persp(pxs 120) <<>> rotX(deg 0) <<>> rotY(deg 0))
            is (per 25)  .> trans (persp(pxs 120) <<>> rotX(neg (deg 180.1)) <<>> rotY(deg 0))
            is (per 50)  .> trans (persp(pxs 120) <<>> rotX(neg (deg 180)) <<>> rotY(neg (deg 179.9)))
            is (per 75)  .> trans (persp(pxs 120) <<>> rotX(deg 0) <<>> rotY(neg(deg 179.9)))
            is (per 100) .> trans (persp(pxs 120) <<>> rotX(deg 0) <<>> rotY(deg 0))

        void $ is c .> do
            width =: pxs w
            height =: pxs h 
            backgroundColor =: toTxt b
            margin =: pxs m <<>> auto
            anim $ Txt.tail c <> " 4s infinite ease-in-out"

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

