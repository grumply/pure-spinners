{-# LANGUAGE OverloadedStrings, GADTs, DataKinds, KindSignatures, TypeApplications, ScopedTypeVariables #-}
module Pure.Spinners.CubeGrid (CubeGrid(..),defaultCubeGrid) where

import Pure.Spinners.Utils

import Pure

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt

import Data.Proxy
import GHC.TypeLits
import Prelude hiding (or)

data CubeGrid :: Nat -> Nat -> Nat -> Nat -> Nat -> Symbol -> * where
  CubeGrid
    :: ( KnownNat duration
       , KnownNat delay
       , KnownNat width
       , KnownNat height
       , KnownNat margin
       , KnownSymbol color
       ) => CubeGrid duration delay width height margin color

instance 
  ( KnownNat duration
  , KnownNat delay
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Theme (CubeGrid duration delay width height margin color)
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

        d', p' :: Double
        d' = fromIntegral d / 1000
        p' = fromIntegral p / 1000

      keyframes (Txt.tail c) $ do
        is (per 0) . or is (per 70) . or is (per 100) .> trans "scale3d(1,1,1)"
        is (per 35) .> trans "scale3d(0,0,1)"

      void $ is c $ do
        apply $ do
          width =: pxs w
          height =: pxs h
          margin =: pxs m <<>> auto

        child (tag Div) $ do
          apply $ do
            width =: per 33.33
            height =: per 33.33
            backgroundColor =: toTxt b
            float =: left
            anim $ Txt.tail c <<>> sec d' <> " infinite ease-in-out"

          for_ (Prelude.zip [1..9] [0.5,0.75,1,0.25,0.5,0.75,0,0.25,0.50]) $ \(n,d) -> 
            is (nth n) .> "animation-delay" =: sec (p' * d)

instance 
  ( KnownNat duration
  , KnownNat delay
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Pure (CubeGrid duration delay width height margin color)
  where
    view _ = Div <| Themed @(CubeGrid duration delay width height margin color) |> Prelude.replicate 9 Div

defaultCubeGrid :: View
defaultCubeGrid = view (CubeGrid :: CubeGrid 1300 400 40 40 40 "#333")