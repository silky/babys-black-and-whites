module WaveAxis where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

pattern :: Diagram B -> Diagram B
pattern d = t
  where
    sizes  = map exp [0.1, 0.3 .. 2]
    t      = mconcat (imap im sizes)
    im k s = if k `mod` 2 == 0 then d # scale s # lw none # fc white
                               else d # scale s # lw none # fc colour

half :: Diagram B
half = pattern (wedge 1 xDir (180 @@ deg))


colour :: Colour Double
colour = black


d :: Diagram B
d = (pad 1.1 f) # bg black
  where
    f      = lower <> upper' <> upper
    upper  = half # snugB
    lower  = half # reflectY # snugT
    upper' = upper # scaleY 0.3
