module Rendering (r) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import System.Environment (withArgs)


r :: Diagram B R2 -> IO ()
r d = withArgs ["-o", "/tmp/diagram1.svg", "-w", "400"] $ mainWith (d # bg black)
