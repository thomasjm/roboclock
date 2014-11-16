module Clock where

import Diagrams.Prelude
import Rendering (r)
import Util

import Data.Maybe
import Control.Monad

import Debug.Trace

-- Settings
l1 = 10
l2 = 10
s = 20


moveCircle p c = translate (r2 . unp2 $ p) c

data DrawingInfo = DrawingInfo { motor1Origin :: P2
                               , motor2Origin :: P2
                               , end1 :: P2
                               , end2 :: P2
                               , pen :: Maybe P2 } deriving Show

bothMotors (DrawingInfo motor1Origin motor2Origin end1 end2 pen) =
    penAndArrows <>
    arrowBetween motor1Origin end1 # lc white <>
    arrowBetween motor2Origin end2 # lc white <>
    moveCircle end1 (circle l2) # lc blue <>
    moveCircle end2 (circle l2) # lc blue
        where penAndArrows = case pen of Nothing -> mempty
                                         Just p -> arrowBetween end1 p # lc green <>
                                                   arrowBetween end2 p # lc green

getDrawingInfo theta1 theta2 = DrawingInfo motor1Origin motor2Origin end1 end2 pen where
    motor1Origin = origin
    motor2Origin = translateX s motor1Origin

    end1 = motor1Origin .+^ (l1 *^ fromDirection theta1)
    end2 = motor2Origin .+^ (l1 *^ fromDirection theta2)

    pen = topIntersectionOfCircles (Circle end1 l2) (Circle end2 l2)


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
