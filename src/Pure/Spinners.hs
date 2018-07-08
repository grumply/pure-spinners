{-# LANGUAGE RecordWildCards, OverloadedStrings, DuplicateRecordFields #-}
module Pure.Spinners where

import Pure

import Pure.Theme
import Pure.Data.CSS
import Pure.Data.Styles
import Pure.Data.Txt as Txt

import Control.Monad
import Data.Foldable hiding (or)
import Data.Monoid

import Prelude hiding (or)

import Data.Typeable

-- A port of https://github.com/tobiasahlin/SpinKit

keyframes nm kfs = do
    atWebkitKeyframes (" " <> nm <> " ") kfs
    atKeyframes (" " <> nm <> " ") kfs

trans t = do
    transform =: t
    "-webkit-transform" =: t

transOrig to = do
    "-webkit-transform-origin" =: to
    "-ms-transform-origin" =: to
    "transform-origin" =: to

anim a = do
    "-webkit-animation" =: a
    animation =: a

persp p = "perspective(" <> p <> ")"
rotX rx = "rotateX(" <> rx <> ")"
rotY ry = "rotateY(" <> ry <> ")"
rotZ rz = "rotateZ(" <> rz <> ")"
nth n = ":nth-child(" <> int n <> ")"

data RotatingPlane = RotatingPlane
  { rpWidth :: Txt
  , rpHeight :: Txt
  , rpBackgroundColor :: Txt
  , rpMargin :: Txt
  }
instance Themeable RotatingPlane where
    theme c RotatingPlane {..} = do
        keyframes (Txt.tail c) $ do
            is (per 0)   .> trans (persp(pxs 120) <<>> rotX(deg 0) <<>> rotY(deg 0))
            is (per 25)  .> trans (persp(pxs 120) <<>> rotX(neg (deg 180.1)) <<>> rotY(deg 0))
            is (per 50)  .> trans (persp(pxs 120) <<>> rotX(neg (deg 180)) <<>> rotY(neg (deg 179.9)))
            is (per 75)  .> trans (persp(pxs 120) <<>> rotX(deg 0) <<>> rotY(neg(deg 179.9)))
            is (per 100) .> trans (persp(pxs 120) <<>> rotX(deg 0) <<>> rotY(deg 0))

        void $ is c .> do
            width =: rpWidth
            height =: rpHeight
            backgroundColor =: rpBackgroundColor
            margin =: rpMargin
            anim $ Txt.tail c <> " 4s infinite ease-in-out"
instance Default RotatingPlane where
    def = RotatingPlane {..}
      where
        rpWidth = pxs 40
        rpHeight = pxs 40
        rpBackgroundColor = "#333"
        rpMargin = pxs 40 <<>> auto
instance Pure RotatingPlane where
    view rp = Div <| Theme rp

data DoubleBounce = DoubleBounce
  { dbWidth :: Txt
  , dbHeight :: Txt
  , dbBackgroundColor :: Txt
  , dbMargin :: Txt
  }
instance Themeable DoubleBounce where
    theme c DoubleBounce {..} = do
        keyframes (Txt.tail c) $ do
            is (per 0) . or is (per 100) .> trans (scale zero)
            is (per 50) .> trans (scale one)

        void $ is c $ do
            apply $ do
                width =: dbWidth
                height =: dbHeight
                margin =: dbMargin
                position =: relative

            child "div" .> do
                width =: per 100
                height =: per 100
                borderRadius =: per 50
                backgroundColor =: dbBackgroundColor
                opacity =: dec 0.6
                position =: absolute
                top =: zero
                left =: zero
                anim $ Txt.tail c <> " 2.0s infinite ease-in-out"

            is ":last-child" .> do
                "animation-delay" =: neg (sec 1)
instance Default DoubleBounce where
    def = DoubleBounce {..}
      where
        dbWidth = pxs 40
        dbHeight = pxs 40
        dbBackgroundColor = "#333"
        dbMargin = pxs 40 <<>> auto
instance Pure DoubleBounce where
    view db = Div <| Theme db |> [ Div, Div ]

data Wave = Wave
  { wRectCount :: Int
  , wAnimationDuration :: Double
  , wDelayRange :: Double
  , wHeight :: Txt
  , wWidth :: Txt
  , wMargin :: Txt
  , wBackgroundColor :: Txt
  }
instance Themeable Wave where
    theme c Wave {..} = do
        keyframes (Txt.tail c) $ do
            is (per 0) . or is (per 40) . or is (per 100) .> trans (scaleY(dec 0.4))
            is (per 20) .> trans (scaleY one)

        void $ is c $ do
            apply $ do
                margin =: wMargin
                height =: wHeight
                width =: "calc(" <> wWidth <> " * 1.25)"
                textAlign =: "center"
                fontSize =: pxs 10

            child "div" $ do
                apply $ do
                    backgroundColor =: wBackgroundColor
                    height =: per 100
                    width =: pxs 6
                    display =: inlineBlock
                    anim $ Txt.tail c <<>> sec wAnimationDuration <> " infinite ease-in-out"

                for_ [1..wRectCount] $ \rect -> is (nth rect) .>
                    "animation-delay" =: neg (sec (wAnimationDuration + wDelayRange / (fromIntegral wRectCount - 1) * (fromIntegral $ wRectCount - rect)))
instance Default Wave where
    def = Wave {..}
      where
        wRectCount = 5
        wAnimationDuration = 1.2
        wDelayRange = 0.4
        wHeight = pxs 40
        wWidth = pxs 40
        wMargin = pxs 40 <<>> auto
        wBackgroundColor = "#333"
instance Pure Wave where
    view wave = Div <| Theme wave |> (Prelude.replicate (wRectCount wave) Div)

data WanderingCubes = WanderingCubes
  { wcAnimationDuration :: Double
  , wcMargin :: Txt
  , wcWidth :: Txt
  , wcHeight :: Txt
  , wcBackgroundColor :: Txt
  , wcCubeDistance :: Txt
  }
instance Themeable WanderingCubes where
    theme c WanderingCubes {..} = do
        keyframes (Txt.tail c) $ do
            is (per 0)    .> trans (rotate(deg 0))
            is (per 25)   .> trans (translateX(wcCubeDistance) <<>> rotate(neg (deg 90)) <<>> scale(dec 0.5))
            is (per 50)   .> trans (translateX(wcCubeDistance) <<>> translateY(wcCubeDistance) <<>> rotate(neg(deg 179)))
            is (per 50.1) .> trans (translateX(wcCubeDistance) <<>> translateY(wcCubeDistance) <<>> rotate(neg(deg 180)))
            is (per 75)   .> trans (translateX(zero) <<>> translateY(wcCubeDistance) <<>> rotate(neg(deg 270)) <<>> scale(dec 0.5))
            is (per 100)  .> trans (rotate(neg(deg 360)))
        void $ is c $ do
            apply $ do
                margin =: wcMargin
                height =: wcHeight
                width =: wcWidth
                position =: relative

            child "div" $ do
                apply $ do
                    backgroundColor =: wcBackgroundColor
                    width =: pxs 10
                    height =: pxs 10
                    position =: absolute
                    top =: zero
                    left =: zero
                    anim $ Txt.tail c <<>> sec wcAnimationDuration <<>> "ease-in-out" <<>> neg (sec wcAnimationDuration) <<>> "infinite both"

                is (nth 2) .>
                    "animation-delay" =: neg (sec (wcAnimationDuration / 2))
instance Default WanderingCubes where
    def = WanderingCubes {..}
      where
        wcAnimationDuration = 1.8
        wcMargin = pxs 40 <<>> auto
        wcWidth = pxs 40
        wcHeight = pxs 40
        wcBackgroundColor = "#333"
        wcCubeDistance = pxs 30
instance Pure WanderingCubes where
    view wc = Div <| Theme wc |> [ Div, Div ]

data Pulse = Pulse
  { pMargin :: Txt
  , pWidth :: Txt
  , pHeight :: Txt
  , pBackgroundColor :: Txt
  }
instance Themeable Pulse where
    theme c Pulse {..} = do
        keyframes (Txt.tail c) $ do
            is (per 0) .> trans (scale zero)
            is (per 100) .> do
                trans $ scale one
                opacity =: zero
        void $ is c .> do
            width =: pWidth
            height =: pHeight
            margin =: pMargin
            backgroundColor =: pBackgroundColor
            borderRadius =: per 100
            anim $ Txt.tail c <<>> sec 1 <> " infinite ease-in-out"
instance Default Pulse where
    def = Pulse {..}
      where
        pDuration = 1
        pMargin = pxs 40 <<>> auto
        pWidth = pxs 40
        pHeight = pxs 40
        pBackgroundColor = "#333"
instance Pure Pulse where
    view p = Div <| Theme p

data ChasingDots = ChasingDots
  { cdAnimationDuration :: Double
  , cdMargin :: Txt
  , cdWidth :: Txt
  , cdHeight :: Txt
  , cdBackgroundColor :: Txt
  }
instance Themeable ChasingDots where
    theme c ChasingDots {..} = do
        keyframes (Txt.tail c <> "_rotate") $
            is (per 100) .> trans (rotate(deg 360))

        keyframes (Txt.tail c <> "_bounce") $ do
            is (per 0) . or is (per 100) .> trans (scale zero)
            is (per 50) .> trans (scale one)

        void $ is c $ do
            apply $ do
                margin =: cdMargin
                height =: cdHeight
                width =: cdWidth
                position =: relative
                textAlign =: "center"
                anim $ Txt.tail c <> "_rotate" <<>> sec cdAnimationDuration <> " infinite linear"

            child "div" .> do
                width =: per 60
                height =: per 60
                display =: inlineBlock
                position =: absolute
                top =: zero
                backgroundColor =: cdBackgroundColor
                borderRadius =: per 100
                anim $ Txt.tail c <> "_bounce" <<>> sec cdAnimationDuration <> " infinite ease-in-out"

            child "div" . is ":last-child" .> do
                top =: auto
                bottom =: zero
                "animation-delay" =: neg (sec (cdAnimationDuration / 2))
instance Default ChasingDots where
    def = ChasingDots {..}
      where
        cdAnimationDuration = 2.0
        cdMargin = pxs 40 <<>> auto
        cdHeight = pxs 40
        cdWidth = pxs 40
        cdBackgroundColor = "#333"
instance Pure ChasingDots where
    view cd = Div <| Theme cd |> [ Div, Div ]

data ThreeBounce = ThreeBounce
  { tbAnimationDuration :: Double
  , tbDelayRange :: Double
  , tbMargin :: Txt
  , tbWidth :: Txt
  , tbHeight :: Txt
  , tbBackgroundColor :: Txt
  }
instance Themeable ThreeBounce where
    theme c ThreeBounce {..} = do
        keyframes (Txt.tail c) $ do
            is (per 0) . or is (per 80) . or is (per 100) .> do
                trans $ scale zero
            is (per 40) .> do
                trans $ scale one
        
        void $ is c $ do
            apply $ do
                margin =: tbMargin
                width =: "calc(" <> tbWidth <> " * 2)"
                textAlign =: "center"

            child "div" $ do
                apply $ do
                    width =: "calc(" <> tbWidth <> " / 2)" 
                    height =: "calc(" <> tbHeight <> " / 2)"
                    backgroundColor =: tbBackgroundColor
                    borderRadius =: per 100
                    display =: inlineBlock
                    anim $ Txt.tail c <<>> sec tbAnimationDuration <> " ease-in-out 0s infinite both"

                is (nth 1) .> "animation-delay" =: neg (sec tbDelayRange)
                is (nth 2) .> "animation-delay" =: neg (sec (tbDelayRange / 2))
instance Default ThreeBounce where
    def = ThreeBounce {..}
      where
        tbAnimationDuration = 1.4
        tbDelayRange = 0.32
        tbMargin = pxs 40 <<>> auto
        tbWidth = pxs 40
        tbHeight = pxs 40
        tbBackgroundColor = "#333"
instance Pure ThreeBounce where
    view tb = Div <| Theme tb |> [ Div, Div, Div ]


data Circle = Circle
  { cCircleCount :: Int
  , cAnimationDuration :: Double
  , cMargin :: Txt
  , cWidth :: Txt
  , cHeight :: Txt
  , cBackgroundColor :: Txt
  }
instance Themeable Circle where
    theme c Circle {..} = do
        keyframes (Txt.tail c) $ do
            is (per 0) . or is (per 80) . or is (per 100) .> trans (scale zero)
            is (per 40) .> trans (scale one)

        void $ is c $ do
            apply $ do
                margin =: cMargin
                width =: cWidth
                height =: cHeight
                position =: relative

            child "div" $ do
                apply $ do
                    width =: per 100
                    height =: per 100
                    position =: absolute
                    left =: zero
                    top =: zero

                is ":before" .> do
                    content =: emptyQuotes
                    display =: block
                    margin =: zero <<>> auto
                    width =: per 15
                    height =: per 15
                    backgroundColor =: cBackgroundColor
                    borderRadius =: per 100
                    anim $ Txt.tail c <<>> sec cAnimationDuration <> " infinite ease-in-out both"

                for_ [2..cCircleCount] $ \c -> is (nth c) $ do
                    apply $ trans $ rotate (deg (360 / fromIntegral cCircleCount * (fromIntegral c - 1)))
                    is ":before" .> "animation-delay" =: neg (sec (cAnimationDuration + cAnimationDuration / fromIntegral cCircleCount * (fromIntegral $ cCircleCount - c)))
instance Default Circle where
    def = Circle {..}
      where
        cCircleCount = 12
        cAnimationDuration = 1.2
        cMargin = pxs 40 <<>> auto
        cWidth = pxs 40
        cHeight = pxs 40
        cBackgroundColor = "#333"
instance Pure Circle where
    view c = Div <| Theme c |> (Prelude.replicate (cCircleCount c) Div)

data CubeGrid = CubeGrid
  { cgAnimationDuration :: Double
  , cgDelayRange :: Double
  , cgMargin :: Txt
  , cgWidth :: Txt
  , cgHeight :: Txt
  , cgBackgroundColor :: Txt
  }
instance Themeable CubeGrid where
    theme c CubeGrid {..} = do
        keyframes (Txt.tail c) $ do
            is (per 0) . or is (per 70) . or is (per 100) .> trans "scale3d(1,1,1)"
            is (per 35) .> trans "scale3d(0,0,1)"

        void $ is c $ do
            apply $ do
                width =: cgWidth
                height =: cgHeight
                margin =: cgMargin

            child "div" $ do
                apply $ do
                    width =: per 33.33
                    height =: per 33.33
                    backgroundColor =: cgBackgroundColor
                    float =: left
                    anim $ Txt.tail c <<>> sec cgAnimationDuration <> " infinite ease-in-out"

                for_ (Prelude.zip [1..9] [0.5,0.75,1,0.25,0.5,0.75,0,0.25,0.50]) $ \(n,d) -> 
                    is (nth n) .> "animation-delay" =: sec (cgDelayRange * d)
instance Default CubeGrid where
    def = CubeGrid {..}
      where
        cgAnimationDuration = 1.3
        cgDelayRange = 0.4
        cgMargin = pxs 40 <<>> auto
        cgWidth = pxs 40
        cgHeight = pxs 40
        cgBackgroundColor = "#333"
instance Pure CubeGrid where
    view c = Div <| Theme c |> (Prelude.replicate 9 Div)
            
data FadingCircle = FadingCircle
   { fcCircleCount :: Int 
   , fcAnimationDuration :: Double
   , fcMargin :: Txt
   , fcHeight :: Txt
   , fcWidth :: Txt
   , fcBackgroundColor :: Txt
   }
instance Themeable FadingCircle where
    theme c FadingCircle {..} = do
        keyframes (Txt.tail c) $ do
            is (per 0) . or is (per 39) . or is (per 100) .> opacity =: zero
            is (per 40) .> opacity =: one

        void $ is c $ do
            apply $ do
                width =: fcWidth
                height =: fcHeight
                margin =: fcMargin
                position =: relative

            child "div" $ do
                apply $ do
                    width =: per 100
                    height =: per 100
                    position =: absolute
                    left =: zero
                    top =: zero

                is ":before" .> do
                    content =: emptyQuotes
                    display =: block
                    margin =: zero <<>> auto
                    width =: per 15
                    height =: per 15
                    backgroundColor =: fcBackgroundColor
                    borderRadius =: per 100
                    anim $ Txt.tail c <<>> sec fcAnimationDuration <> " infinite ease-in-out both"

                for_ [2..fcCircleCount] $ \c -> is (nth c) $ do
                    apply $ trans $ rotate (deg (360 / fromIntegral fcCircleCount * (fromIntegral c - 1)))
                    is ":before" .> "animation-delay" =: neg (sec (fcAnimationDuration + fcAnimationDuration / fromIntegral fcCircleCount * (fromIntegral $ fcCircleCount - c)))
instance Default FadingCircle where
    def = FadingCircle {..}
      where
        fcCircleCount = 12
        fcAnimationDuration = 1.2
        fcMargin = pxs 40 <<>> auto
        fcHeight = pxs 40
        fcWidth = pxs 40
        fcBackgroundColor = "#333"
instance Pure FadingCircle where
    view fc = Div <| Theme fc |> (Prelude.replicate (fcCircleCount fc) Div)

data FoldingCube = FoldingCube
  { focAnimationDuration :: Double 
  , focDelayRange :: Double
  , focMargin :: Txt
  , focWidth :: Txt
  , focHeight :: Txt
  , focBackgroundColor :: Txt
  }
instance Themeable FoldingCube where
    theme c FoldingCube {..} = do
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
                margin =: focMargin
                width =: focWidth
                height =: focHeight
                position =: relative
                trans $ rotZ(deg 45)

            child "div" $ do
                apply $ do
                    float =: left
                    width =: per 50
                    height =: per 50
                    position =: relative
                    trans $ scale(dec 1.1)
                
                is ":before" .> do
                    content =: emptyQuotes
                    position =: absolute
                    top =: zero
                    left =: zero
                    width =: per 100
                    height =: per 100
                    backgroundColor =: focBackgroundColor
                    anim $ Txt.tail c <<>> sec focAnimationDuration <> " infinite linear both"
                    transOrig $ per 100 <<>> per 100

                for_ [(2,1),(3,3),(4,2)] $ \(n,i) ->
                    is (nth n) $ do
                        apply $ trans $ scale(dec 1.1) <<>> rotZ(deg $ 90 * fromIntegral i)
                        is ":before" .> "animation-delay" =: sec (focDelayRange / 4 * fromIntegral i)
instance Default FoldingCube where
    def = FoldingCube {..}
      where
        focAnimationDuration = 2.4
        focDelayRange = 1.2
        focMargin = pxs 40 <<>> auto
        focWidth = pxs 40
        focHeight = pxs 40
        focBackgroundColor = "#333"
instance Pure FoldingCube where
    view foc = Div <| Theme foc |> [ Div, Div, Div, Div ]