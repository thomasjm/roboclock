module Rendering where

import Diagrams.Prelude
-- import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.Cairo.CmdLine

import System.Environment (withArgs)

import Text.Printf

type FrameList = [Diagram B R2]

renderSingleFrame :: String -> Diagram B R2 -> IO ()
renderSingleFrame s d = withArgs ["-o", s, "-w", "800"] $ mainWith (d # bg black)

renderFrameList :: FrameList -> IO ()
renderFrameList frameList = sequence_ ioActions where
    filenames = [printf "/tmp/frame%d.png" x | x <- [1..(length frameList)]]
    ioActions = zipWith renderSingleFrame filenames frameList

r = renderSingleFrame "/tmp/diagram1.svg"
