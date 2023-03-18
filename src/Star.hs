-- https://diagrams.github.io/gallery/Star.html
module Star where

import Diagrams.Prelude hiding (connect)

colors = [ black, white ]

quarter n = mconcat [arrowBetween'
  (with & arrowHead .~ noHead
        & shaftStyle %~ lw 50 . lc (colors !! ((xCoord1 p) `mod` 2)))
  (fst p) (snd p) | p <- ps]
  where
    xCoord1 = round . fst . unp2 . fst
    ps = zip xs (reverse ys)
    (xs, ys) = pts n

d n = r <> rotateBy (1/16) r
  where
    half = (rotateBy (1/4) q ||| q) # centerX
    q = quarter n
    f = half === rotateBy (1/2) half
    r = f <> rotateBy (1/8) f

pts n = (map (p2 . (,0)) [0..n], map (p2 . (0,)) [0..n])

example = pad 1.1 $ d 10 # centerXY
