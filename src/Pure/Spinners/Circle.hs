{-# LANGUAGE OverloadedStrings, GADTs, DataKinds, KindSignatures, TypeApplications, ScopedTypeVariables #-}
module Pure.Spinners.Circle (Circle(..),defaultCircle) where

import Pure.Spinners.Utils

import Pure

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt

import Data.Proxy
import GHC.TypeLits
import Prelude hiding (or)

data Circle :: Nat -> Nat -> Nat -> Nat -> Nat -> Symbol -> * where
  Circle
    :: ( KnownNat count
       , KnownNat duration
       , KnownNat width
       , KnownNat height
       , KnownNat margin
       , KnownSymbol color
       ) => Circle count duration width height margin color

instance 
  ( KnownNat count
  , KnownNat duration
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Theme (Circle count duration width height margin color)
  where
    theme c = do
      let
        n, d, w, h, m :: Int
        n = fromIntegral $ natVal @count Proxy 
        d = fromIntegral $ natVal @duration Proxy 
        w = fromIntegral $ natVal @width Proxy 
        h = fromIntegral $ natVal @height Proxy 
        m = fromIntegral $ natVal @margin Proxy 

        b :: String
        b = symbolVal @color Proxy 

        d' :: Double
        d' = fromIntegral d / 1000

      keyframes (Txt.tail c) $ do
          is (per 0) . or is (per 80) . or is (per 100) .> trans (scale zero)
          is (per 40) .> trans (scale one)

      void $ is c $ do
        apply $ do
          margin =: pxs m <<>> auto
          width =: pxs w
          height =: pxs h
          position =: relative

        child (tag Div) $ do
          apply $ do
            width =: per 100
            height =: per 100
            position =: absolute
            left =: zero
            top =: zero

          is before .> do
            content =: emptyQuotes
            display =: block
            margin =: zero <<>> auto
            width =: per 15
            height =: per 15
            backgroundColor =: toTxt b
            borderRadius =: per 100
            anim $ Txt.tail c <<>> ms d <> " infinite ease-in-out both"

          for_ [2..n] $ \i -> is (nth i) $ do
            apply $ trans $ rotate (deg (360 / fromIntegral n * (fromIntegral i - 1)))
            is before .> "animation-delay" =: neg (sec (d' + d' / fromIntegral n * (fromIntegral $ n - i)))

instance
  ( KnownNat count
  , KnownNat duration
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Pure (Circle count duration width height margin color)
  where
    view _ = 
      let c = fromIntegral $ natVal @count Proxy
      in Div <| Themed @(Circle count duration width height margin color) |> Prelude.replicate c Div

defaultCircle :: View
defaultCircle = view (Circle :: Circle 12 1200 40 40 40 "#333")
