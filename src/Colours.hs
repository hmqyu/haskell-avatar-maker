module Colours ( colourToRGBA8, dyeImage, dyeAllImages ) where

---------------
--- IMPORTS ---
---------------
import Codec.Picture


---------------
-- CONSTANTS --
---------------
colourToRGBA8 :: String -> PixelRGBA8
colourToRGBA8 "white"       = PixelRGBA8 255 255 255 255
colourToRGBA8 "red"         = PixelRGBA8 204 90 83 255
colourToRGBA8 "green"       = PixelRGBA8 100 209 108 255
colourToRGBA8 "blue"        = PixelRGBA8 100 169 209 255
colourToRGBA8 "blonde"      = PixelRGBA8 230 214 167 255
colourToRGBA8 "light brown" = PixelRGBA8 178 136 110 255
colourToRGBA8 "chocolate brown"  = PixelRGBA8 100 81 79 255
colourToRGBA8 "black brown"       = PixelRGBA8 65 60 61 255
colourToRGBA8 "pale"   = PixelRGBA8 255 223 201 255
colourToRGBA8 "light"     = PixelRGBA8 244 195 161 255
colourToRGBA8 "tan"  = PixelRGBA8 204 154 128 255
colourToRGBA8 "brown"    = PixelRGBA8 164 108 53 255
colourToRGBA8 "dark brown"    = PixelRGBA8 149 96 68 255
colourToRGBA8 "black"    = PixelRGBA8 84 62 54 255
colourToRGBA8 _ = PixelRGBA8 0 0 0 0


---------------
-- FUNCTIONS --
---------------
-- dyes an image using a given colour
dyeImage :: (Image PixelRGBA8, PixelRGBA8) -> Image PixelRGBA8
dyeImage (img, PixelRGBA8 r1 g1 b1 a1) =
  pixelMap (\(PixelRGBA8 r2 g2 b2 a2) -> if a2 == 0 then PixelRGBA8 r2 g2 b2 a2 else PixelRGBA8 r1 g1 b1 a1) img

-- dyes all images using the given list of colours
dyeAllImages :: [Image PixelRGBA8] -> [PixelRGBA8] -> [Image PixelRGBA8]
dyeAllImages images colours = map dyeImage (zip images colours)