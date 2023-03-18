-- https://diagrams.github.io/gallery/VectorField.html
module VectorField where

import Diagrams.Prelude

locs   = [(x, y) | x <- [0.1, s .. m], y <- [0.1, s .. m]]
  where
    s = 0.9
    m = ((s - 0.1) * 3)

points = map p2 locs

vectorField (x, y) = r2 (sin (y*y*y + 1), sin (x/y + 1))

arrows = map arrowAtPoint locs

arrowAtPoint (x, y) = arrowAt' opts (p2 (x, y)) (sL *^ vf) # alignTL
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

example = example' # centerXY
