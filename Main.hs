module Main where

import Diagrams.Prelude

import Clock
import Rendering


main = r $ bothMotors $ getDrawingInfo (3,3,3,3) (50 @@ deg) (130 @@ deg)
