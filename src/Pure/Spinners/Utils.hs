{-# language OverloadedStrings #-}
module Pure.Spinners.Utils where

import Pure.Theme

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

