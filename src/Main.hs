module Main (main) where

import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.Offset
import Diagrams.TwoD.Path.Boolean qualified as Boolean
-- import System.Random


main :: IO ()
main = mainWith haskellLogo


haskellLogo :: Diagram B
haskellLogo =
  Path trails
    # expandPath 0.2
    # Boolean.union Winding
    # stroke
    # centerXY
  where
    verts  = (map . map) p2 haskellPoints
    trails = map fromVertices verts
    haskellPoints
      = [ -- >
        [ (0, 3), (1.2, 1.5), (0,0) ]
        -- \
        , [ (0.8, 3), (0.8 + (2 * 1.2), 0) ]
        -- /
        , [ (0.8, 0), (0.8 + 1.2, 1.5) ]
        -- =
        , [ (2.2, 1.85), (4, 1.85) ]
        , [ (2.7, 1.32), (4, 1.32) ]
        ]
