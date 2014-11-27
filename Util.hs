module Util where

import Diagrams.Prelude

import Diagrams.TwoD.Vector

import Data.List (maximumBy)
import Data.Function (on)

import Debug.Trace

data Circle = Circle { circOrigin :: P2
                     , circRadius :: Double } deriving Show

moveCircle p c = translate (r2 . unp2 $ p) c

rectAtOrigin w h = translate (r2 (w/2.0, h/2.0)) (rect w h)

-- Only works on equal-radius circles
topIntersectionOfCircles :: Circle -> Circle -> Maybe P2
topIntersectionOfCircles c1@(Circle p1 radius1) c2@(Circle p2 radius2)
    | radius1 /= radius2 = error "This function only works on equal-radius circles"
    | magnitude (2*d) > radius1 + radius2 = Nothing
    | otherwise = Just pen
    where
      d = (p2 .-. p1) / 2

      -- We know the intersection we want satisfies
      -- |d|**2 + |p|**2 = radius1**2
      h = sqrt $ radius1**2 - (magnitude d)**2
      v = h *^ (normalized $ perp d)

      -- Take the one with the larger y-value
      pen = maximumBy (compare `on` (snd . unp2)) [p1 .+^ (d + v), p1 .+^ (d - v)]

-- Floating a => a -> a -> a -> a
linspace :: Double -> Double -> Int -> [Double]
linspace start end num = map (\x -> start + fromIntegral x * dx) [0..(num-1)] where
    dx = ((end - start) / fromIntegral (num-1))

    -- scanl (+) start (replicate num ((end - start) / fromIntegral num))
