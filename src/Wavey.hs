module Wavey where

import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

type2 :: Diagram B
type2 = pad 1.2 (d # centerXY)
  where
    d :: Diagram B
    d = hsep 0.3 (map r' [1..5])
          -- # fc red
          -- # lc blue
          # lw 50
      where
        r :: Path V2 Double
        r = fromOffsets $ [unitY] # scaleY 2

        r' k = r # deform' 0.001 (wibble k)
                 # strokeP
                 # fc black


    wibble :: Double -> Deformation V2 V2 Double
    wibble k = Deformation $ \p -> (x p ^& y p)
        where
          xy p = abs $ (p ^. _x) - (p ^. _y)
          x p  = (p ^. _x) + f * cos ((xy p) * tau + m * k)
          y p  = (p ^. _y) + f * sin ((xy p) * tau + m * k)
          f = 0.09
          m = 2/tau

type1 :: Diagram B
type1 = d # rotateBy (1/4)
  where
    d :: Diagram B
    d = hsep 0.002 (map r' [1..100])
          # fc red
          # lc (sRGB24read "4169e1")
      where
        r :: Path V2 Double
        r = fromOffsets $ [unitY]

        r' k = r # deform' 0.001 (wibble k)
              # strokeP


    wibble :: Double -> Deformation V2 V2 Double
    wibble k = Deformation $ \p ->
      ((p^._x) + f * cos ((p ^. _y) * tau + m * k)) ^& ((p ^. _y) + f * sin ((p ^. _x) * tau + m * k))
        where
          f = 0.02
          m = 3/tau

grid =
  ( type1 # lw 0.4 # scaleY 0.5 # centerXY <>
    (type1 # rotateBy (-1/4)
                # lw 0.4
                # scaleX 0.4
                -- # scaleY 1.5
                # centerXY)
  ) # centerXY
         # clipTo (square 0.5 # centerXY)
