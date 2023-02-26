module AvatarMaker
    ( hairColours, hairLengths, hairTextures, skinColours, eyeColours, copyPixels ) where

---------------
--- IMPORTS ---
---------------
import Codec.Picture


---------------
-- CONSTANTS --
---------------
hairColours :: [String]
hairColours = ["black", "brown", "honey", "blonde"]

hairLengths :: [String]
hairLengths = ["short","medium","long"] 

hairTextures :: [String]
hairTextures = ["straight","wavy","curly"] 

skinColours :: [String]
skinColours = ["light","tan","medium", "dark"] 

eyeColours :: [String]
eyeColours = ["blue","green","hazel", "brown"]

-- colour constants --

colourToRGBA8 :: String -> Maybe PixelRGBA8
colourToRGBA8 "white"       = Just $ PixelRGBA8 255 255 255 255
colourToRGBA8 "red"         = Just $ PixelRGBA8 240 83 83 255
colourToRGBA8 "green"       = Just $ PixelRGBA8 100 209 108 255
colourToRGBA8 "blue"        = Just $ PixelRGBA8 100 169 209 255
colourToRGBA8 "blonde"      = Just $ PixelRGBA8 230 214 167 255
colourToRGBA8 "light brown" = Just $ PixelRGBA8 178 136 110 255
colourToRGBA8 "dark brown"  = Just $ PixelRGBA8 100 81 79 255
colourToRGBA8 "black"       = Just $ PixelRGBA8 65 60 61 255
colourToRGBA8 _             = Nothing


skinColourToRGBA8 :: String -> Maybe PixelRGBA8
skinColourToRGBA8 "light"   = Just $ PixelRGBA8 181 140 104 255
skinColourToRGBA8 "tan"     = Just $ PixelRGBA8 244 195 161 255
skinColourToRGBA8 "medium"  = Just $ PixelRGBA8 204 154 128 255
skinColourToRGBA8 "dark"    = Just $ PixelRGBA8 134 90 78 255
skinColourToRGBA8 _         = Nothing



----------------
---- FIELDS ----
----------------



---------------
--- METHODS ---
---------------
-- TODO:
-- createAvatar 
-- creates the avatar using the given components

-- TODO: done???
-- mergeImages
-- merges two PNG images together into one PNG image
-- the first image passed will be merged on top of the second image passed

copyPixels :: Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8
copyPixels fromImage toImage = generateImage genFunc newWidth newHeight
  where
    genFunc x y = if pixelOpacity (pixelAt fromImage x y) < 255
        then pixelAt toImage x y
        -- then mixPixel (pixelAt fromImage x y) (pixelAt toImage x y)
        else pixelAt fromImage x y
    newWidth = imageWidth toImage
    newHeight = imageHeight toImage

-- for smoothening the sharp edges
-- mixPixel :: PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
-- mixPixel (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
--     PixelRGBA8
--         (mixPixelValue r1 r2 a1)
--         (mixPixelValue g1 g2 a1)
--         (mixPixelValue b1 b2 a1)
--         (mixPixelValue a1 a2 a1)

-- mixPixelValue :: (Integral a) => a -> a -> a -> a
-- -- mixPixelValue c1 c2 alpha = div ((255 - alpha) 255) * c1 + (alpha / 255) * c2
-- mixPixelValue c1 c2 alpha = round ((fromIntegral (255 - alpha) / 255.0) * fromIntegral c1 + (fromIntegral alpha / 255.0) * fromIntegral c2)

-- TODO:
dyeImage :: Image PixelRGBA8 -> PixelRGBA8 -> Image PixelRGBA8
dyeImage img (PixelRGBA8 r1 g1 b1 a1) =
  pixelMap (\(PixelRGBA8 r2 g2 b2 a2) -> if a2 == 0 then PixelRGBA8 r2 g2 b2 a2 else PixelRGBA8 r1 g1 b1 a2) img
-- dyes an image either using a defined colour constant or a given RGB/hexcode value