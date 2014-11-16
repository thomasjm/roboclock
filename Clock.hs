module Clock where

import Diagrams.Prelude
import Rendering (r)

l1 = 10
l2 = 5

motor direction = arrow # fc blue
                  <>
                  circ # lc blue
                      where
    arrow = arrowAt origin end1
    end1 = l1 *^ fromDirection direction
    circ = translate end1 (circle l2)

bothMotors dir1 dir2 = position
                       [(origin, motor dir1),
                        (translateX 20 origin, motor dir2)]

data Circle = Circle { circOrigin :: P2
                     , circRadius :: Double }

topIntersectionOfCircles :: Circle -> Circle -> Maybe R2
topIntersectionOfCircles = undefined
