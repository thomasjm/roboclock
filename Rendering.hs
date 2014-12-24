{-# LANGUAGE OverloadedStrings #-}
module Rendering where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
-- import Diagrams.Backend.Cairo.CmdLine -- This one can render PNGs

import System.Environment (withArgs)

import Text.Printf
import Shelly

import Data.Text (pack)

type FrameList = [Diagram B R2]

renderSingleFrame :: String -> Diagram B R2 -> IO ()
renderSingleFrame s d = withArgs ["-o", s, "-w", "800"] $ mainWith (d # bg white)

renderFrameList :: FrameList -> IO ()
renderFrameList frameList = do
  putStrLn "Rendering frames as SVGs"
  sequence_ $ zipWith renderSingleFrame svgFilenames frameList

  putStrLn "Converting SVGs to PNGs"
  _ <- shelly $ escaping False $ run "rm" ["-f", "/tmp/frame*.png"]
  sequence_ $ zipWith convertAction svgFilenames pngFilenames
  _ <- shelly $ escaping False $ run "rm" ["/tmp/frame*.svg"]

  putStrLn $ "Generating animated gif at " ++ gifFilename
  _ <- shelly $ run "convert" ["-loop", "0", "/tmp/frame*.png", pack gifFilename]

  return ()
    where
    svgFilenames = [printf "/tmp/frame%05d.svg" x | x <- [1..(length frameList)]]
    pngFilenames = [printf "/tmp/frame%05d.png" x | x <- [1..(length frameList)]]
    gifFilename = "/tmp/animation.gif"

    convertAction = (\x y -> shelly $ run "convert" $ map pack [x, y])

r = renderSingleFrame "/tmp/diagram1.svg"
