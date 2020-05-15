{-# LANGUAGE OverloadedStrings, GADTs, DataKinds, KindSignatures, TypeApplications, ScopedTypeVariables #-}
module Pure.Spinners.DoubleBounce (DoubleBounce(..),defaultDoubleBounce) where

import Pure.Spinners.Utils

import Pure

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt

import Data.Proxy
import GHC.TypeLits
import Prelude hiding (or)

data DoubleBounce :: Nat -> Nat -> Nat -> Symbol -> * where
  DoubleBounce 
    :: (KnownNat width, KnownNat height, KnownNat margin, KnownSymbol color) 
    => DoubleBounce width height margin color

instance 
  ( KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Theme (DoubleBounce width height margin color) 
  where
    theme c = do
      let 
        w, h, m :: Int
        w = fromIntegral $ natVal @width Proxy 
        h = fromIntegral $ natVal @height Proxy 
        m = fromIntegral $ natVal @margin Proxy 

        b :: String
        b = symbolVal (Proxy @color)

      keyframes (Txt.tail c) $ do
        is (per 0) . or is (per 100) .> trans (scale zero)
        is (per 50) .> trans (scale one)

      void $ is c $ do
        apply $ do
          width =: pxs w
          height =: pxs h
          margin =: pxs m <<>> auto
          position =: relative

        child (tag Div) .> do
          width =: per 100
          height =: per 100
          borderRadius =: per 50
          backgroundColor =: toTxt b
          opacity =: dec 0.6
          position =: absolute
          top =: zero
          left =: zero
          anim $ Txt.tail c <> " 2.0s infinite ease-in-out"

        pseudo "last-child" .> do
          "animation-delay" =: neg (sec 1)

instance 
  ( KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Pure (DoubleBounce width height margin color) 
  where
    view _ = Div <| Themed @(DoubleBounce width height margin color) |> [ Div, Div ]

defaultDoubleBounce :: View
defaultDoubleBounce = view (DoubleBounce :: DoubleBounce 40 40 40 "#333")

