module Experiments where

import Diagrams.Prelude

import Clock
import Util

import Data.Maybe

drawSweep n = (foldl1 (<>) $ map (flip moveCircle $ (circle 0.05 # fc yellow # lc yellow)) validPoints)
           <> moveCircle m1origin (circle 0.5 # fc red)
           <> moveCircle m2origin (circle 0.5 # fc red)
           <> bothMotors firstValid

    where
      sweepInfos = [getDrawingInfo (theta1 @@ deg) (theta2 @@ deg) | theta1 <- (linspace 0 360 n), theta2 <- (linspace 0 360 n)]
      validPoints = catMaybes (map pen sweepInfos)

      firstValid = head $ filter (isJust . pen) sweepInfos
      m1origin = motor1Origin firstValid
      m2origin = motor2Origin firstValid


drawSweep' n =
    foldl1 (<>) $ map bothMotors validInfos

    where
      sweepInfos = [getDrawingInfo (theta1 @@ deg) (theta2 @@ deg) | theta1 <- (linspace 0 360 n), theta2 <- (linspace 0 360 n)]
      validInfos = filter (isJust . pen) sweepInfos
