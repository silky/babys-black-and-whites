module Moons where

import Diagrams.Prelude
import qualified Diagrams.TwoD.Path.Boolean as Boolean

locs   = [(x, y) | x <- [0.1, s .. m], y <- [0.1, s .. m]]
  where
    s = 0.9
    m = ((s - 0.1) * 2)

points = map p2 locs

vectorField (x, y) = r2 (sin (y + 1), sin (x + 1))

arrows = map arrowAtPoint locs

arrowAtPoint (x, y) =
  moon (m/3)
    # scale (0.5 * m)
    # rotateBy m
    # moveTo (p2 (x, y))
  where
    vf   = vectorField (x, y)
    m    = norm $ vectorField (x, y)

    hs   = 0.11 * m
    sW   = 0.03 * m
    sL   = 0.40 + 0.2 * m
    opts = (with & arrowHead  .~ spike
                 & headLength .~ normalized hs
                 & shaftStyle %~ lwN sW)

field   = position $ zip points arrows

example' = ( field # translateY 0.1 # centerXY )
        <> ( square 3.2 # lw none)

manyMoons = pad 1.1 $ example' # centerXY


oneBigMoon = pad 1.1 (moon 0.4 # centerXY)

moon m = d # strokeP # recommendFillColor black # lw 0
  where
    c1 = circle 1
    c2 = circle 0.7 # moveTo (p2 (0.3 + m, 0))
    d  = Boolean.difference Winding c1 c2
