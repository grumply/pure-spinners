{-# LANGUAGE OverloadedStrings, GADTs, DataKinds, KindSignatures, TypeApplications, ScopedTypeVariables #-}
module Pure.Spinners.ChasingDots (ChasingDots(..),defaultChasingDots) where

import Pure.Spinners.Utils

import Pure

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt

import Data.Proxy
import GHC.TypeLits
import Prelude hiding (or)

data ChasingDots :: Nat -> Nat -> Nat -> Nat -> Symbol -> * where
  ChasingDots
    :: ( KnownNat duration
       , KnownNat width
       , KnownNat height
       , KnownNat margin
       , KnownSymbol color
       ) => ChasingDots duration width height margin color

instance
  ( KnownNat duration
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Theme (ChasingDots duration width height margin color) 
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

      keyframes (Txt.tail c <> "_rotate") $
        is (per 100) .> trans (rotate(deg 360))

      keyframes (Txt.tail c <> "_bounce") $ do
        is (per 0) . or is (per 100) .> trans (scale zero)
        is (per 50) .> trans (scale one)

      void $ is c $ do
        apply $ do
          margin =: pxs m <<>> auto
          height =: pxs h
          width =: pxs w
          position =: relative
          textAlign =: "center"
          anim $ Txt.tail c <> "_rotate" <<>> ms d <> " infinite linear"

        child (tag Div) .> do
          width =: per 60
          height =: per 60
          display =: inlineBlock
          position =: absolute
          top =: zero
          backgroundColor =: toTxt b
          borderRadius =: per 100
          anim $ Txt.tail c <> "_bounce" <<>> ms d <> " infinite ease-in-out"

        child (tag Div) . pseudo "last-child" .> do
          top =: auto
          bottom =: zero
          "animation-delay" =: neg (ms (d `div` 2))

instance
  ( KnownNat duration
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Pure (ChasingDots duration width height margin color) 
  where
    view _ = 
      Div <| Themed @(ChasingDots duration width height margin color) |> 
        [ Div, Div ]

defaultChasingDots :: View
defaultChasingDots = view (ChasingDots :: ChasingDots 2000 40 40 40 "#333")
