module Colours
    ( colourToRGBA8, dyeImage ) where

---------------
--- IMPORTS ---
---------------
import Codec.Picture


---------------
-- CONSTANTS --
---------------
colourToRGBA8 :: String -> PixelRGBA8
colourToRGBA8 "white"       = PixelRGBA8 255 255 255 255
colourToRGBA8 "red"         = PixelRGBA8 240 83 83 255
colourToRGBA8 "green"       = PixelRGBA8 100 209 108 255
colourToRGBA8 "blue"        = PixelRGBA8 100 169 209 255
colourToRGBA8 "blonde"      = PixelRGBA8 230 214 167 255
colourToRGBA8 "light brown" = PixelRGBA8 178 136 110 255
colourToRGBA8 "dark brown"  = PixelRGBA8 100 81 79 255
colourToRGBA8 "black"       = PixelRGBA8 65 60 61 255
colourToRGBA8 "light"   = PixelRGBA8 255 223 201 255
colourToRGBA8 "tan"     = PixelRGBA8 244 195 161 255
colourToRGBA8 "medium"  = PixelRGBA8 204 154 128 255
colourToRGBA8 "dark"    = PixelRGBA8 134 90 78 255
colourToRGBA8 _ = PixelRGBA8 0 0 0 0


---------------
-- FUNCTIONS --
---------------
-- dyes an image using a given colour
dyeImage :: (Image PixelRGBA8, PixelRGBA8) -> Image PixelRGBA8
dyeImage (img, PixelRGBA8 r1 g1 b1 a1) =
  pixelMap (\(PixelRGBA8 r2 g2 b2 a2) -> if a2 == 0 then PixelRGBA8 r2 g2 b2 a2 else PixelRGBA8 r1 g1 b1 a1) img