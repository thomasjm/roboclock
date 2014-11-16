module Main where

import Diagrams.Prelude

import Clock
import Rendering


main = r $ bothMotors (50 @@ deg) (120 @@ deg)
