{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module CubicLimitSeries where

import Diagrams.Prelude
import Diagrams.TwoD.Tilings
import Data.List
import Data.List.Split (chunksOf)
import System.Random (randomRIO, randomR, mkStdGen, newStdGen)
import Control.Monad
-- import Data.Random.RVar
-- import Data.Random
import Diagrams.Backend.Cairo.CmdLine
import System.Random.Shuffle (shuffle')


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
-- linesToDraw = circleParts 150
-- linesToDraw = circleParts 50
linesToDraw = cubeParts


linesWith :: [[P2 Double]] -> Colour Double -> [Int] -> Diagram B
linesWith sublines c indicies =
    sublines'
        # map fromVertices
        # mconcat
        # centerXY
    <> p
    where
        p :: Diagram B
        p = (square 3) # lw 0.2 # lc c

        -- Always everything
        -- sublines' = sublines
        --
        -- Some subset
        sublines' = map (\i -> sublines !! i) indicies


tile :: [[P2 Double]]
     -> Colour Double
     -> Colour Double
     -> Colour Double
     -> [Int]
     -> Diagram B
tile lines c1 c2 c3 xs
  = linesWith lines c2 xs
      # lw 20
      # lc c1
      # bg c3


someIndicies :: IO [Int]
someIndicies = do
    gen <- newStdGen
    totalIndicies <- randomRIO (0, length linesToDraw - 1)

    let shuffled = shuffle' [0..length linesToDraw - 1] (length linesToDraw - 1) gen

    let indicies = take totalIndicies shuffled

    return indicies


-- Let's do something else. Let's pick a random set of indicies,
-- then increment it randomly. Either by:
--
--  1. Removing some element
--  2. Adding some element
--
nextSet :: [[P2 Double]] -> [Int] -> IO [Int]
nextSet lines xs = do
    b :: Int <- randomRIO (0, 1)
    i <- randomRIO (0, length xs - 1)

    let gen = mkStdGen 1
    let remaining = [0..length lines - 1] \\ xs
    let shuffled = shuffle' remaining (length remaining) gen

    let forcedDrop = length xs == length lines

    let xs' = if length xs > 1 && (b == 0 || forcedDrop)
                then dropIndex i xs
                else (xs ++ take 1 shuffled)

    return xs'


dropIndex :: Int -> [a] -> [a]
dropIndex _ [] = []
dropIndex 0 xs = drop 1 xs
dropIndex i xs = let (h, t) = splitAt i xs in h ++ dropIndex i (drop 1 t)



pdesign :: [[P2 Double]]
        -> Int
        -> Bool
        -> Colour Double
        -> IO (Diagram B)
pdesign lines n vary bg = do
    let chunks = n
        items  = chunks * chunks

    -- Method 1:
    --  Random variations in each step.
    --
    let stepDifference = 1
    let f (cur, xs) _  = do xs' <- foldM (\nxs _ -> nextSet lines nxs) xs [1..stepDifference]
                            return (xs' : cur, xs')

    seqs'   <- foldM f (return [], [1, 2]) [0.. items -1] >>= return . fst

    let seqs = if vary then seqs' else replicate n [0 .. length lines - 1]

    -- Colour ones:
    -- let getTile s cs = tile black (head cs) (head cs) s
    -- let getTile s cs = tile (head cs) (head cs) black s
    let getTile s cs = tile lines (head cs) bg bg s
    --
    -- Original one:
    let tiles        = zipWith getTile seqs [[black]]


    -- Method 2:
    --  Just completely random.
    --
    -- tiles <- replicateM items (someIndicies >>= return . tile')

    let diag = vcat (map hcat (chunksOf chunks tiles))
                -- # clipTo (square 2.1) --  :: Diagram B)

    return diag




design
  = do
    d1 <- pdesign lines n vary bg
    d2 <- pdesign lines n vary bg
    d3 <- pdesign lines n vary bg
    d4 <- pdesign lines n vary bg
    pure $ (d1 ||| d2) === (d3 ||| d4)
  where
    lines = linesToDraw
    n     = 10
    vary  = True
    bg    = white
