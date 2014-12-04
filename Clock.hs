{-# LANGUAGE ForeignFunctionInterface #-}
module Clock where

import Diagrams.Prelude
import Rendering
import Util

import Data.Maybe
import Control.Monad

import Debug.Trace

import Foreign
import Foreign.C.Types

import Numbers (numbers)

-- Settings
l1 = 10.0
l2 = 10.0
s = 20.0 :: Double
colonSpace = 1.5
boxWidth = (s - colonSpace - (2*sideGap)) / 4
boxHeight = 8.0
boxY = 6.0
sideGap = 1.5

boxOrigin :: Int -> R2
boxOrigin n = r2 (x, y) where
    x = sideGap + (boxWidth * fromIntegral n) + (if n >= 2 then colonSpace else 0)
    y = 4.0

data DrawingInfo = DrawingInfo { motor1Origin :: P2
                               , motor2Origin :: P2
                               , end1 :: P2
                               , end2 :: P2
                               , pen :: Maybe P2 } deriving Show

getNumberSeries :: Int -> R2 -> Double -> Double -> [P2]
getNumberSeries n offset scaleX scaleY = map
                                         (\(x,y) -> (p2 (x * scaleX, y * scaleY)) .+^ offset)
                                         samples where
    number = numbers !! n
    samples = map number [x / 100.0 | x <- [0..100]]


bothMotors (DrawingInfo motor1Origin motor2Origin end1 end2 pen) =
    -- Pen position, if one was provided
    case pen of Nothing -> mempty
                Just p -> arrowBetween end1 p # lc green <>
                          arrowBetween end2 p # lc green

    -- White lines from motor origin to rod 1 endpoints
    <> arrowBetween motor1Origin end1 # lc white
    <> arrowBetween motor2Origin end2 # lc white

    -- Blue lines from rod 1 endpoints to rod 2 endpoints
    <> moveCircle end1 (circle l2) # lc blue
    <> moveCircle end2 (circle l2) # lc blue

    -- Boxes for number drawing
    <> translate (boxOrigin 0) (rectAtOrigin boxWidth boxHeight # lc white)
    <> translate (boxOrigin 1) (rectAtOrigin boxWidth boxHeight # lc white)
    <> translate (boxOrigin 2) (rectAtOrigin boxWidth boxHeight # lc white)
    <> translate (boxOrigin 3) (rectAtOrigin boxWidth boxHeight # lc white)

    -- Numbers in the boxes
    <> drawNumberPoints 0 9
    <> drawNumberPoints 1 3
    <> drawNumberPoints 2 3
    <> drawNumberPoints 3 3

makeLetterAnimation :: Int -> Int -> FrameList
makeLetterAnimation number boxNum = diagrams
    where
      points = getNumberSeries number (boxOrigin boxNum) boxWidth boxHeight
      diagrams = map (bothMotors . (uncurry getDrawingInfo) . findThetas) points

drawNumberPoints boxNum n = foldl1 (<>) dots where
    dots = [circleAt x | x <- getNumberSeries n (boxOrigin boxNum) boxWidth boxHeight]
    circleAt x = translate (r2 $ unp2 x) (circle 0.005 # fc yellow # lc yellow)


getDrawingInfo theta1 theta2 = DrawingInfo motor1Origin motor2Origin end1 end2 pen where
    motor1Origin = origin
    motor2Origin = translateX s motor1Origin

    end1 = motor1Origin .+^ (l1 *^ fromDirection theta1)
    end2 = motor2Origin .+^ (l1 *^ fromDirection theta2)

    pen = topIntersectionOfCircles (Circle end1 l2) (Circle end2 l2)


foreign import ccall "theta.h sol1"
    sol1 :: CDouble -> CDouble -> CDouble -> CDouble
foreign import ccall "theta.h sol2"
    sol2 :: CDouble -> CDouble -> CDouble -> CDouble
findThetas :: P2 -> (Angle, Angle)
findThetas p = (min (s11 @@ rad) (s12 @@ rad),
                min (s21 @@ rad) (s22 @@ rad))
    where
    (x, y) = unp2 p

    c = (l2*l2) / (l1*l1)

    -- Compute the two theta values for the first motor
    a = x / l1
    b = y / l1
    s11 = realToFrac $ sol1 (realToFrac a) (realToFrac b) (realToFrac c)
    s12 = realToFrac $ sol2 (realToFrac a) (realToFrac b) (realToFrac c)

    -- Compute the two theta values for the second motor, which is translated
    othera = (x - s) / l1
    s21 = realToFrac $ sol1 (realToFrac othera) (realToFrac b) (realToFrac c)
    s22 = realToFrac $ sol2 (realToFrac othera) (realToFrac b) (realToFrac c)
