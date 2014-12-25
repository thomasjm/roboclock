module Numbers (TraceFn, numbers, traceFnFromPoints) where

import Data.SVG.SVG
import System.IO.Unsafe (unsafePerformIO)

import Data.List
import Data.Maybe

import Diagrams.Prelude ((.-.), (^*), (.+^), magnitude, p2, unp2, lerp)
import Graphics.SVG.ReadPath

import Text.HTML.TagSoup

import Debug.Trace

type TraceFn = (Double -> (Double, Double))

numbers :: [TraceFn]
numbers = map makeNum [ "/Users/tomm/Dropbox/haskell/clock/numbers/9_onestroke.svg"
                      , "/Users/tomm/Dropbox/haskell/clock/numbers/9_onestroke.svg"
                      , "/Users/tomm/Dropbox/haskell/clock/numbers/9_onestroke.svg"
                      , "/Users/tomm/Dropbox/haskell/clock/numbers/3_onestroke.svg"
                      , "/Users/tomm/Dropbox/haskell/clock/numbers/9_onestroke.svg"
                      , "/Users/tomm/Dropbox/haskell/clock/numbers/9_onestroke.svg"
                      , "/Users/tomm/Dropbox/haskell/clock/numbers/9_onestroke.svg"
                      , "/Users/tomm/Dropbox/haskell/clock/numbers/9_onestroke.svg"
                      , "/Users/tomm/Dropbox/haskell/clock/numbers/9_onestroke.svg"
                      , "/Users/tomm/Dropbox/haskell/clock/numbers/9_onestroke.svg" ]

makeNum = fromJust . makeTraceFn . unsafePerformIO . readFile

findTagAndGetAttrs :: String -> [Tag String] -> Maybe [Attribute String]
findTagAndGetAttrs tagName (TagOpen s attrs : xs) | s == tagName = Just attrs
                                                  | otherwise = findTagAndGetAttrs tagName xs
findTagAndGetAttrs tagName (_:xs) = findTagAndGetAttrs tagName xs


extractPathString :: [Tag String] -> Maybe String
extractPathString tags = findTagAndGetAttrs "path" tags >>= lookup "d"

extractDims :: [Tag String] -> Maybe (Double, Double)
extractDims tags = do
  attrs <- findTagAndGetAttrs "svg" tags
  case (lookup "width" attrs, lookup "height" attrs) of
    (Just w, Just h) -> Just (read w, read h)
    _ -> Nothing

-- The library writer felt the need to make pathFromString return IO [PathCommand]
-- just so he could print out errors to stdout when the parse failed
nonStupidPathFromString :: String -> Maybe [PathCommand]
nonStupidPathFromString = Just . unsafePerformIO . pathFromString

-- Take an SVG file as a string and make a TraceFn
-- The SVG is expected to contain a single "path" element, which will be used to derive the TraceFn.
-- The TraceFn will be rescaled to be a function from [0,1] -> [0,1] X [0,1]
makeTraceFn :: String -> Maybe TraceFn
makeTraceFn s = do
    -- Extract the dimensions and path string
    let tags = parseTags s
    (maxX, maxY) <- extractDims tags
    pathString <- extractPathString tags

    -- Parse out the path commands
    pathCommands <- nonStupidPathFromString pathString

    let unscaledPts = head $ commandsToPoints pathCommands (1,1) (0,0)
        pts = map (\(x, y) -> (x / maxX, (maxY - y) / maxY)) unscaledPts

    return $ traceFnFromPoints pts


traceFnFromPoints :: [(Double, Double)] -> Double -> (Double, Double)
traceFnFromPoints pts t | t < 0 = head pts
                        | t > 1 = last pts
                        | otherwise = unp2 answer where

  points = map p2 pts
  displacements = map (\(x, y) -> y .-. x) $ zip points (tail points)
  distanceCdf = scanl (+) 0 $ map magnitude displacements

  -- Find the point we're looking for in the distanceCdf and interpolate
  dist = t * (last distanceCdf)
  answer = case findIndex (> dist) distanceCdf of
    -- If we run off the end, just return the last point
    Nothing -> last points
    -- Otherwise interpolate. TODO: figure out how to use the VectorSpace lerp function
    Just i -> (points !! (i-1)) .+^ ((displacements !! (i-1)) ^* (remainingDistance / distanceBetweenPoints)) where
      remainingDistance = dist - (distanceCdf !! (i-1))
      distanceBetweenPoints = magnitude $ (displacements !! (i-1))
