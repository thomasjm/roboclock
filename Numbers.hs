module Numbers (TraceFn, numbers) where

import Data.SVG.SVG
import System.IO.Unsafe (unsafePerformIO)

import Data.Maybe

import Graphics.SVG.ReadPath
import Debug.Trace

import Text.HTML.TagSoup

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


-- This will be hacky for now -- use actual interpolation later
traceFnFromPoints :: [(Double, Double)] -> Double -> (Double, Double)
traceFnFromPoints pts t = pts !! (min (l-1) index) where
    l = length pts
    index = floor $ t * fromIntegral l
