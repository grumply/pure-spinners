{-# LANGUAGE OverloadedStrings, GADTs, DataKinds, KindSignatures, TypeApplications, ScopedTypeVariables #-}
module Pure.Spinners.ThreeBounce (ThreeBounce(..),defaultThreeBounce) where

import Pure.Spinners.Utils

import Pure

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt

import Data.Proxy
import GHC.TypeLits
import Prelude hiding (or)

data ThreeBounce :: Nat -> Nat -> Nat -> Nat -> Nat -> Symbol -> * where
  ThreeBounce
    :: ( KnownNat duration
       , KnownNat delay
       , KnownNat width
       , KnownNat height
       , KnownNat margin
       , KnownSymbol color
       ) => ThreeBounce duration delay width height margin color

instance
  ( KnownNat duration
  , KnownNat delay
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Theme (ThreeBounce duration delay width height margin color)
  where
    theme c = do
      let
        d, p, w, h, m :: Int
        d = fromIntegral $ natVal @duration Proxy 
        p = fromIntegral $ natVal @delay Proxy 
        w = fromIntegral $ natVal @width Proxy 
        h = fromIntegral $ natVal @height Proxy 
        m = fromIntegral $ natVal @margin Proxy 

        b :: String
        b = symbolVal @color Proxy

      keyframes (Txt.tail c) $ do
          is (per 0) . or is (per 80) . or is (per 100) .> do
              trans $ scale zero
          is (per 40) .> do
              trans $ scale one
      
      void $ is c $ do
          apply $ do
              margin =: pxs m <<>> auto
              width =: "calc(" <> pxs w <> " * 2)"
              textAlign =: "center"

          child (tag Div) $ do
              apply $ do
                  width =: "calc(" <> pxs w <> " / 2)" 
                  height =: "calc(" <> pxs h <> " / 2)"
                  backgroundColor =: toTxt b
                  borderRadius =: per 100
                  display =: inlineBlock
                  anim $ Txt.tail c <<>> ms d <> " ease-in-out 0s infinite both"

              is (nth 1) .> "animation-delay" =: neg (ms p)
              is (nth 2) .> "animation-delay" =: neg (ms (p `div` 2))

instance
  ( KnownNat duration
  , KnownNat delay
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Pure (ThreeBounce duration delay width height margin color)
  where
    view _ = 
      Div <| Themed @(ThreeBounce duration delay width height margin color) |>
        [ Div, Div, Div ]

defaultThreeBounce :: View
defaultThreeBounce = view (ThreeBounce :: ThreeBounce 1400 320 40 40 40 "#333")
