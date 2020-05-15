{-# LANGUAGE OverloadedStrings, GADTs, DataKinds, KindSignatures, TypeApplications, ScopedTypeVariables #-}
module Pure.Spinners.FoldingCube (FoldingCube(..),defaultFoldingCube) where

import Pure.Spinners.Utils

import Pure

import Pure.Theme
import Pure.Data.Styles
import Pure.Data.Txt as Txt

import Data.Proxy
import GHC.TypeLits
import Prelude hiding (or)

data FoldingCube :: Nat -> Nat -> Nat -> Nat -> Nat -> Symbol -> * where
  FoldingCube
    :: ( KnownNat duration
       , KnownNat delay
       , KnownNat width
       , KnownNat height
       , KnownNat margin
       , KnownSymbol color
       ) => FoldingCube duration delay width height margin color

instance 
  ( KnownNat duration
  , KnownNat delay
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Theme (FoldingCube duration delay width height margin color) 
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

        d',p' :: Double
        d' = fromIntegral d / 1000
        p' = fromIntegral p / 1000

      keyframes (Txt.tail c) $ do
        is (per 0) . or is (per 10) .> do
          trans $ persp(pxs 140) <<>> rotX(neg (deg 180))
          opacity =: zero
        is (per 25) . or is (per 75) .> do
          trans $ persp(pxs 140) <<>> rotX(deg 0)
          opacity =: one
        is (per 90) . or is (per 100) .> do
          trans $ persp(pxs 140) <<>> rotY(deg 180)
          opacity =: zero
        
      void $ is c $ do
        apply $ do
          margin =: pxs m <<>> auto
          width =: pxs w
          height =: pxs h
          position =: relative
          trans $ rotZ(deg 45)

        child (tag Div) $ do
          apply $ do
            float =: left
            width =: per 50
            height =: per 50
            position =: relative
            trans $ scale(dec 1.1)
            
          is before .> do
            content =: emptyQuotes
            position =: absolute
            top =: zero
            left =: zero
            width =: per 100
            height =: per 100
            backgroundColor =: toTxt b
            anim $ Txt.tail c <<>> sec d' <> " infinite linear both"
            transOrig $ per 100 <<>> per 100

          for_ [(2,1),(3,3),(4,2)] $ \(n,i) ->
            is (nth n) $ do
              apply $ trans $ scale(dec 1.1) <<>> rotZ(deg $ 90 * fromIntegral i)
              is before .> "animation-delay" =: sec (p' / 4 * fromIntegral i)

instance 
  ( KnownNat duration
  , KnownNat delay
  , KnownNat width
  , KnownNat height
  , KnownNat margin
  , KnownSymbol color
  ) => Pure (FoldingCube duration delay width height margin color) 
  where
    view _ = Div <| Themed @(FoldingCube duration delay width height margin color) |> [ Div, Div, Div, Div ]

defaultFoldingCube :: View
defaultFoldingCube = view (FoldingCube :: FoldingCube 2400 1200 40 40 40 "#333")