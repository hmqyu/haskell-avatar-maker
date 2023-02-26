module AvatarMaker
    ( hairColours, hairLengths, hairTextures, skinColours, eyeColours, createAvatar ) where

---------------
--- IMPORTS ---
---------------
import Codec.Picture

---------------
-- CONSTANTS --
---------------
avatarParts :: [String]
avatarParts = ["hair", "eyes", "skin"]

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

----------------
---- FIELDS ----
----------------

---------------
--- METHODS ---
---------------
-- TODO:
-- createAvatar 
-- creates the avatar using the given components
createAvatar :: [Image PixelRGBA8] -> Image PixelRGBA8
createAvatar (h:t) = foldl (\x y -> mergeImages y x) h t

-- TODO: done???
-- mergeImages
-- merges two PNG images together into one PNG image
-- the first image passed will be merged on top of the second image passed

mergeImages :: Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8
mergeImages topImage bottomImage = generateImage replacePixel width height
  where
    replacePixel x y 
        | pixelOpacity (pixelAt topImage x y) == 0 = pixelAt bottomImage x y
        | pixelOpacity (pixelAt topImage x y) < 255 = mixPixel (pixelAt topImage x y) (pixelAt bottomImage x y)
        | otherwise = pixelAt topImage x y
    width = imageWidth bottomImage
    height = imageHeight bottomImage

-- deals with pixels that have transparency by determining what colour the pixel should be if a pixel with transparency is layered on top\
-- values ending with 1 are layered on top of values ending with 2
mixPixel :: PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
mixPixel (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
    if a2 == 0 then PixelRGBA8 r1 g1 b1 a1
    else PixelRGBA8
        (mixPixelValue r1 r2 a1)
        (mixPixelValue g1 g2 a1)
        (mixPixelValue b1 b2 a1)
        (mixPixelValue a1 a2 a1)

-- determines what colour value it should be if a colour with transparency is layered on top
mixPixelValue :: (Integral a) => a -> a -> a -> a
mixPixelValue c1 c2 alpha = round ((fromIntegral alpha / 255.0) * fromIntegral c1 + (fromIntegral (255 - alpha) / 255.0) * fromIntegral c2)

-- TODO:
-- dyeImage
-- dyes an image either using a defined colour constant or a given RGB/hexcode value