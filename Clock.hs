{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Clock where

import Diagrams.Prelude
import Rendering
import Util

import Data.List (minimumBy)
import Data.Function (on)

import Data.Maybe
import Control.Monad

import Debug.Trace

-- import Foreign
import Foreign.C.Types

import Numbers (numbers)

-- Settings
l1 = 13.0
l2 = 13.0
s = 20.0 :: Double
colonSpace = 1.5
boxWidth = (s - colonSpace - (2*sideGap)) / 4
boxHeight = 7.0
boxY = l1
sideGap = -2.0
numSamples = 30

circleAt x = translate (r2 $ unp2 x) (circle 0.005 # fc red # lc red)


boxOrigin :: Int -> R2
boxOrigin n = r2 (x, y) where
    x = sideGap + (boxWidth * fromIntegral n) + (if n >= 2 then colonSpace else 0)
    y = boxY

data DrawingInfo = DrawingInfo { motor1Origin :: P2
                               , motor2Origin :: P2
                               , end1 :: P2
                               , end2 :: P2
                               , pen :: Maybe P2
                               , nums :: (Int, Int, Int, Int) } deriving Show

getNumberSeries :: Int -> R2 -> Double -> Double -> Int -> [P2]
getNumberSeries n offset scaleX scaleY numSamples =
  map (\(x,y) -> (p2 (x * scaleX, y * scaleY)) .+^ offset) samples where
    number = numbers !! n
    samples = map number [(fromIntegral x) / (fromIntegral numSamples) | x <- [0..numSamples]]


bothMotors :: Renderable (Path R2) b => DrawingInfo -> Diagram b R2
bothMotors (DrawingInfo motor1Origin motor2Origin end1 end2 pen (n1, n2, n3, n4)) =
    -- Pen position, if one was provided
    case pen of Nothing -> mempty
                Just p -> arrowBetween end1 p # lc green <>
                          arrowBetween end2 p # lc green

    -- Black lines from motor origin to rod 1 endpoints
    <> arrowBetween motor1Origin end1 # lc black
    <> arrowBetween motor2Origin end2 # lc black

    -- Blue circles at rod 1 endpoints
    -- <> moveCircle end1 (circle l2) # lc blue
    -- <> moveCircle end2 (circle l2) # lc blue

    -- Boxes for number drawing
    <> translate (boxOrigin 0) (rectAtOrigin boxWidth boxHeight # lc black)
    <> translate (boxOrigin 1) (rectAtOrigin boxWidth boxHeight # lc black)
    <> translate (boxOrigin 2) (rectAtOrigin boxWidth boxHeight # lc black)
    <> translate (boxOrigin 3) (rectAtOrigin boxWidth boxHeight # lc black)

    -- Numbers in the boxes
    <> drawNumberPoints 0 n1
    <> drawNumberPoints 1 n2
    <> drawNumberPoints 2 n3
    <> drawNumberPoints 3 n4

drawReachablePoints :: Renderable (Path R2) b => Diagram b R2
drawReachablePoints = (bothMotors $ getDrawingInfo (0,0,0,0) (0 @@ deg) (0 @@ deg))
                      <> dots where
  theta1 = map (@@ deg) [90..180]
  theta2 = map (@@ deg) [0..90]

  thetaPairs = [(x,y) | x <- theta1, y <- theta2]
  drawingInfos = map (uncurry $ getDrawingInfo (0,0,0,0)) thetaPairs
  validPens = catMaybes $ map pen drawingInfos :: [P2]
  dots = foldl1 (<>) [circleAt x | x <- validPens]


makeLettersAnimation :: (Int, Int, Int, Int) -> FrameList
makeLettersAnimation nums@(n1,n2,n3,n4) =
  concat $ zipWith (makeLetterAnimation nums) [n1,n2,n3,n4] [0..3]

makeLetterAnimation :: (Int, Int, Int, Int) -> Int -> Int -> FrameList
makeLetterAnimation nums numToDraw boxNum = diagrams
    where
      points = getNumberSeries numToDraw (boxOrigin boxNum) boxWidth boxHeight numSamples
      diagrams = map (bothMotors . (uncurry $ getDrawingInfo nums) . findThetas) points

drawNumberPoints boxNum n = foldl1 (<>) dots where
    dots = [circleAt x | x <- getNumberSeries n (boxOrigin boxNum) boxWidth boxHeight numSamples]

getDrawingInfo nums theta1 theta2 = DrawingInfo motor1Origin motor2Origin end1 end2 pen nums where
    motor1Origin = translateX (s/2) origin
    motor2Origin = motor1Origin

    end1 = motor1Origin .+^ (l1 *^ fromDirection theta1)
    end2 = motor2Origin .+^ (l1 *^ fromDirection theta2)

    pen = topIntersectionOfCircles (Circle end1 l2) (Circle end2 l2)


closerTo :: Angle -> Angle -> Angle -> Angle
closerTo desired a1 a2 = minimumBy (compare `on` dist) [a1, a2] where
    dist a = angleBetween (rotate desired unitX) (rotate a unitX)

closerTo180 = closerTo (180 @@ deg)
closerTo0 = closerTo (0 @@ deg)


foreign import ccall "theta.h sol1"
    sol1 :: CDouble -> CDouble -> CDouble -> CDouble
foreign import ccall "theta.h sol2"
    sol2 :: CDouble -> CDouble -> CDouble -> CDouble
findThetas :: P2 -> (Angle, Angle)
findThetas p = (closerTo180 (s1 @@ rad) (s2 @@ rad),
                closerTo0 (s1 @@ rad) (s2 @@ rad))
    where
    (x, y) = unp2 p

    c = (l2*l2) / (l1*l1)

    -- Compute the two theta values for the motor
    a = (x - (s/2)) / l1
    b = y / l1
    s1 = realToFrac $ sol1 (realToFrac a) (realToFrac b) (realToFrac c)
    s2 = realToFrac $ sol2 (realToFrac a) (realToFrac b) (realToFrac c)
