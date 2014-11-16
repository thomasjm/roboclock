{-# LANGUAGE NoMonomorphismRestriction #-}
import Diagrams.Prelude

blueCircle = circle 1 # fc blue
                      # lw veryThick
                      # lc purple
                      # dashingG [0.2,0.05] 0

twoCircles = circle 1 # fc red # lw none ||| circle 1 # fc blue # lw none
