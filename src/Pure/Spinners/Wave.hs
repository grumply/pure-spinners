{-# LANGUAGE OverloadedStrings, GADTs, DataKinds, KindSignatures, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes #-}
module Pure.Spinners.Wave (Wave(..),defaultWave) where

import Pure.Spinners.Utils

import Pure

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt

import Data.Proxy
import GHC.TypeLits
import Prelude hiding (or)

data Wave :: Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Symbol -> * where
    Wave :: 
        ( KnownNat count
        , KnownNat duration
        , KnownNat delay
        , KnownNat height
        , KnownNat width
        , KnownNat margin
        , KnownSymbol color
        ) => Wave count duration delay height width margin color


instance 
  ( KnownNat count
  , KnownNat duration
  , KnownNat delay
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Theme (Wave count duration delay width height margin color) 
  where
    theme c = do
      let
        n, d, w, h, m :: Int
        n = fromIntegral $ natVal @count Proxy
        d = fromIntegral $ natVal @duration Proxy
        p = fromIntegral $ natVal @delay Proxy
        w = fromIntegral $ natVal @width Proxy
        h = fromIntegral $ natVal @height Proxy
        m = fromIntegral $ natVal @margin Proxy

        n',d',p' :: Double
        n' = fromIntegral n
        d' = fromIntegral d / 1000
        p' = fromIntegral p / 1000

        b :: String
        b = symbolVal @color Proxy

      keyframes (Txt.tail c) $ do
        is (per 0) . or is (per 40) . or is (per 100) .> trans (scaleY(dec 0.4))
        is (per 20) .> trans (scaleY one)

      void $ is c $ do
        apply $ do
          margin =: pxs m <<>> auto
          height =: pxs h
          width =: "calc(" <> pxs w <> " * 1.25)"
          textAlign =: "center"
          fontSize =: pxs 10

        child (tag Div) $ do
          apply $ do
            backgroundColor =: toTxt b
            height =: per 100
            width =: pxs 6
            display =: inlineBlock
            anim $ Txt.tail c <<>> ms d <> " infinite ease-in-out"

          for_ [1..n] $ \r -> is (nth r) .>
            "animation-delay" =: neg (sec (d' + p' / (n' - 1) * (n' - fromIntegral r)))

instance 
  ( KnownNat count
  , KnownNat duration
  , KnownNat delay
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Pure (Wave count duration delay width height margin color) 
  where
    view _ = 
        let c = fromIntegral $ natVal @count Proxy 
        in Div <| Themed @(Wave count duration delay width height margin color) |> Prelude.replicate c Div

defaultWave :: View
defaultWave = view (Wave :: Wave 5 1200 400 40 40 40 "#333")
