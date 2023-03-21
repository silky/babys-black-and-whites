{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module CubicLimitSeries where

import Control.Monad
import Control.Monad.Random (getRandomR, evalRand, RandomGen, Rand)
import Data.List
import Data.List.Split (chunksOf)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.Tilings
import System.Random (randomR, mkStdGen, newStdGen)
import System.Random.Shuffle (shuffleM)
import qualified Diagrams.TwoD.Path.Boolean as Boolean
import Diagrams.TwoD.Offset (expandPath)

squareParts :: [[P2 Double]]
squareParts =
    [ [ 0 ^& 0, 0 ^& 1 ]
    , [ 0 ^& 1, 1 ^& 1 ]
    , [ 1 ^& 1, 1 ^& 0 ]
    , [ 1 ^& 0, 0 ^& 0 ]
    ]


almostCubeParts :: ([[P2 Double]], [[P2 Double]])
almostCubeParts = (squareParts, (map offset squareParts))
    where
        offset [a, b] = [a + amount, b + amount]
        amount = 0.5


-- An cube (the original)
cubeParts :: [[P2 Double]]
cubeParts = as ++ bs ++ zipWith g as bs
    where
        g [a, _] [c, _] = [a, c]
        (as, bs) = almostCubeParts


-- The rays of a unit circle
circleParts :: Double -> [[P2 Double]]
circleParts n = map f [0..n]
    where
        f k = [ 0 ^& 0, cos (k * a * pi / 180) ^& sin (k * a * pi / 180) ]
        a = 360 / n




linesToDraw :: [[P2 Double]]
linesToDraw = cubeParts


linesWith :: [[P2 Double]] -> Colour Double -> [Int] -> Diagram B
linesWith sublines c indicies =
    sublines'
        # map fromVertices
        # mconcat
        # expandPath 0.2
        # stroke
        # centerXY
        # fc black
    <> p
    where
        p :: Diagram B
        p = (square 3) # lw 0.2 # lc c
        sublines' = map (\i -> sublines !! i) indicies


tile :: [[P2 Double]]
     -> Colour Double
     -> Colour Double
     -> Colour Double
     -> [Int]
     -> Diagram B
tile lines c1 c2 c3 xs
  = linesWith lines c2 xs


-- Let's do something else. Let's pick a random set of indicies,
-- then increment it randomly. Either by:
--
--  1. Removing some element
--  2. Adding some element
--
nextSet :: RandomGen g => [[P2 Double]] -> [Int] -> Rand g [Int]
nextSet lines xs = do
  b <- getRandomR (0, 1 :: Int)
  i <- getRandomR (0, length xs - 1)

  let remaining = [0..length lines - 1] \\ xs

  shuffled <- shuffleM remaining

  let forcedDrop = length xs == length lines

  let xs' = if length xs > 1 && (b == 0 || forcedDrop)
              then dropIndex i xs
              else (xs ++ take 1 shuffled)

  pure xs'


dropIndex :: Int -> [a] -> [a]
dropIndex _ [] = []
dropIndex 0 xs = drop 1 xs
dropIndex i xs = let (h, t) = splitAt i xs in h ++ dropIndex i (drop 1 t)


pdesign
  :: RandomGen g
  => [[P2 Double]]
  -> Int
  -> Bool
  -> Colour Double
  -> Rand g (Diagram B)
pdesign lines n vary bg = do
    let chunks         = n
        items          = chunks * chunks
        stepDifference = 1

    let f (cur, xs) _ =
          do xs' <- foldM (\nxs _ -> nextSet lines nxs) xs [1 .. stepDifference]
             return (xs' : cur, xs')

    seqs' <- foldM f (return [], [1, 2]) [0.. items -1] >>= return . fst

    let seqs = if vary then seqs' else replicate n [0 .. length lines - 1]

    let getTile s cs = tile lines (head cs) bg bg s
        tiles        = zipWith getTile seqs [[black]]
        diag         = vcat (map hcat (chunksOf chunks tiles))

    return diag


design
  = do
    let gen = mkStdGen 2
    flip evalRand gen $ do
      d1 <- pdesign lines n vary bg'
      d2 <- pdesign lines n vary bg'
      d3 <- pdesign lines n vary bg'
      d4 <- pdesign lines n vary bg'
      pure $ (d1 ||| d2) === (d3 ||| d4)
  where
    lines = linesToDraw
    n     = 10
    vary  = True
    bg'   = white
