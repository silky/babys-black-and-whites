-- https://diagrams.github.io/gallery/DiamondTheory.html
module DiamondTheory where

import System.Random
import Data.List.Split
import Diagrams.Prelude

side = sqrt 2

triangleRect = polygon ( with
  & polyType .~ PolySides
     [ 135 @@ deg, 90 @@ deg]
     [ 1        , side      ]
  )

triangleLeft = triangleRect # rotateBy (1/2) # fc white # lc white # lw none

triangleRight = triangleRect # fc black #lc black # lw none

smallTile = beside (r2 (1,-1)) (triangleLeft # align (r2 (1, -1)))
                                triangleRight

smallTile' x = smallTile # rotate x'
  where x' = fromIntegral x *pi/2 @@ rad

createMatrix x = matrix # alignX 0 # alignY 0
  where matrix = (x !! 0 ||| x !! 1 )
                         ===
                 (x !! 2 ||| x !! 3)

mediumTile angles = createMatrix (map smallTile' angles)

largeTile angles xSymmetry ySymmetry = createMatrix [a, b, c, d]
  where
    a = mediumTile $ chunks !! 0
    b = if ySymmetry then a # reflectX else mediumTile $ chunks !! 1
    c = if xSymmetry then a # reflectY else mediumTile $ chunks !! 2
    d
      | ySymmetry && xSymmetry = a # rotateBy (-1/2)
      | ySymmetry  = c # reflectX
      | xSymmetry  = b # reflectY
      | otherwise = mediumTile $ chunks !! 3
    chunks = chunksOf 4 angles


-- Needs a list of 16 angles and the number of axes
-- largeTile' :: ([Int], Int) -> Diagram B
largeTile' x = largeTile n xSymmetry ySymmetry
  where
    n = fst x
    nbAxes = snd x
    xSymmetry = nbAxes == 1 || nbAxes == 3
    ySymmetry = nbAxes == 2 || nbAxes == 3

tile = largeTile' (angles, nbAxes)
  where
    angles = randInts 4
    nbAxes = 3

randInts :: Int -> [Int]
randInts seed = randomRs (0, 3) (mkStdGen seed)
