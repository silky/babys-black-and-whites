module Main (main) where

import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

import Moons         qualified as Moons
import Star          qualified as Star
import VectorField   qualified as VectorField
import DiamondTheory qualified as DiamondTheory

main :: IO ()
-- main = mainWith haskellLogo
-- main = mainWith (VectorField.example :: Diagram B)
-- main = mainWith (Star.example :: Diagram B)
-- main = mainWith (Moons.oneBigMoon :: Diagram B)
-- main = mainWith (reflectX $ Moons.oneBigMoon :: Diagram B)
-- main = mainWith circles
-- main = mainWith (Moons.moons :: Diagram B)
-- main = mainWith (Moons.manyMoons :: Diagram B)
-- main = mainWith (DiamondTheory.tile :: Diagram B)
main = mainWith (undefined :: Diagram B)


circles :: Diagram B
circles = pad 1.1
        $ mconcat
        $ reverse
        $ zipWith circ pts (cycle [black, white])
  where
    pts = [1, 0.9 .. 0.0]
    circ n c = circle n # fc c # lw none


haskellLogo :: Diagram B
haskellLogo = d <> (square 5 # lw none)
  where
    d = (mempty
        <> lambdas
        <> eqsD # translateX (1.1)
        ) # centerXY
    lambdas
      = (asPath haskellPoints)
          # stroke
          # lw 50
          # centerXY
          # clipBy (square 5 # translateY (1.2))
          # clipBy (square 5 # translateY (-1.2))
    eqs =
      [ [ (2.3, 1.89), (4, 1.89) ]
        , [ (2.7, 1.30), (4, 1.30) ]
      ]
    eqsP = Path $ map fromVertices $ (map . map) p2 eqs
    eqsD = mempty
            <> x # stroke # lc white # lw 50 # translateY (0.2)
            <> eqsP # stroke # lw 50 # lc black # centerXY
    x = asPath [farLine'] # centerXY # translateX (-0.4) # translateY (-0.4)
    asPath = Path . map fromVertices . (map . map) p2
    farLine' = [ (0, 3/2), (0 + (1 * 1.2), 0) ]
    farLine = [ (0.8, 3), (0.8 + (2 * 1.2), 0) ]
    haskellPoints
      = [ -- >
        [ (0, 3), (1.2, 1.5), (0,0) ]
        -- \
        , farLine
        -- /
        , [ (0.8, 0), (0.8 + 1.2, 1.5) ]
        ]
