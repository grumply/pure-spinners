{-# LANGUAGE OverloadedStrings, GADTs, DataKinds, KindSignatures, TypeApplications, ScopedTypeVariables #-}
module Pure.Spinners.Pulse (Pulse(..),defaultPulse) where

import Data.Proxy
import Pure.Spinners.Utils

import Pure

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt

import GHC.TypeLits
import Prelude

data Pulse :: Nat -> Nat -> Nat -> Nat -> Symbol -> * where
  Pulse
    :: ( KnownNat duration
       , KnownNat width
       , KnownNat height
       , KnownNat margin
       , KnownSymbol color
       ) => Pulse duration width height margin color

instance
  ( KnownNat duration
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Theme (Pulse duration width height margin color)
  where
    theme c = do
      let
        d, w, h, m :: Int
        d = fromIntegral $ natVal @duration Proxy 
        w = fromIntegral $ natVal @width Proxy 
        h = fromIntegral $ natVal @height Proxy 
        m = fromIntegral $ natVal @margin Proxy 

        b :: String
        b = symbolVal @color Proxy 

      keyframes (Txt.tail c) $ do
        is (per 0) .> trans (scale zero)
        is (per 100) .> do
          trans $ scale one
          opacity =: zero

      void $ is c .> do
        width =: pxs w
        height =: pxs h
        margin =: pxs m <<>> auto
        backgroundColor =: toTxt b
        borderRadius =: per 100
        anim $ Txt.tail c <<>> ms d <> " infinite ease-in-out"

instance
  ( KnownNat duration
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Pure (Pulse duration width height margin color)
  where
    view _ = Div <| Themed @(Pulse duration width height margin color)

defaultPulse :: View
defaultPulse = view (Pulse :: Pulse 1000 40 40 40 "#333")
