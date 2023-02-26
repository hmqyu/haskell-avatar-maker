module AvatarMaker
    ( AvatarPart(..), apOptions, createAvatar ) where

---------------
--- IMPORTS ---
---------------
import Codec.Picture


---------------
-- CONSTANTS --
---------------
-- avatar part constants --
data AvatarPart = HairColour | HairLength | HairTexture | HasBangs | EyeColour | SkinColour | ShirtColour
    deriving (Eq, Show)
apOptions :: AvatarPart -> [String]
apOptions HairColour = ["black", "light brown", "dark brown", "blonde"]
apOptions HairLength = ["short","medium","long"]
apOptions HairTexture = ["straight","wavy","curly"]
apOptions HasBangs = ["yes","no"]
apOptions EyeColour = ["blue","green","light brown", "dark brown"]
apOptions SkinColour = ["light","tan","medium", "dark"]
apOptions ShirtColour = ["blue","green","red","black", "white"]

-- colour constants --
colourToRGBA8 :: String -> PixelRGBA8
colourToRGBA8 "white"       = PixelRGBA8 255 255 255 255
colourToRGBA8 "red"         = PixelRGBA8 240 83 83 255
colourToRGBA8 "green"       = PixelRGBA8 100 209 108 255
colourToRGBA8 "blue"        = PixelRGBA8 100 169 209 255
colourToRGBA8 "blonde"      = PixelRGBA8 230 214 167 255
colourToRGBA8 "light brown" = PixelRGBA8 178 136 110 255
colourToRGBA8 "dark brown"  = PixelRGBA8 100 81 79 255
colourToRGBA8 "black"       = PixelRGBA8 65 60 61 255
colourToRGBA8 "light"   = PixelRGBA8 181 140 104 255
colourToRGBA8 "tan"     = PixelRGBA8 244 195 161 255
colourToRGBA8 "medium"  = PixelRGBA8 204 154 128 255
colourToRGBA8 "dark"    = PixelRGBA8 134 90 78 255
colourToRGBA8 _ = PixelRGBA8 0 0 0 0


---------------
--- METHODS ---
---------------
-- TODO:
-- createAvatar 
-- creates the avatar using the given components
createAvatar :: [Image PixelRGBA8] -> [Image PixelRGBA8] -> [String] -> Image PixelRGBA8
createAvatar flats linearts colours = mergeAllImages (weaveLayers (dyeAvatarParts flats (convertStringToPixelRGBA8 colours)) linearts)

weaveLayers :: [Image PixelRGBA8] -> [Image PixelRGBA8] -> [Image PixelRGBA8]
weaveLayers flats linearts = foldr (\(flat, lineart) y -> [flat, lineart] ++ y) [] (zip flats linearts)

mergeAllImages :: [Image PixelRGBA8] -> Image PixelRGBA8
mergeAllImages (h:t) = foldl (\x y -> mergeImages y x) h t

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

dyeAvatarParts :: [Image PixelRGBA8] -> [PixelRGBA8] -> [Image PixelRGBA8]
dyeAvatarParts images colours = map dyeImage (zip images colours)

-- dyes an image either using a defined colour constant
dyeImage :: (Image PixelRGBA8, PixelRGBA8) -> Image PixelRGBA8
dyeImage (img, PixelRGBA8 r1 g1 b1 a1) =
  pixelMap (\(PixelRGBA8 r2 g2 b2 a2) -> if a2 == 0 then PixelRGBA8 r2 g2 b2 a2 else PixelRGBA8 r1 g1 b1 a1) img

convertStringToPixelRGBA8 :: [String] -> [PixelRGBA8]
convertStringToPixelRGBA8 colours = [colourToRGBA8 x | x <- colours]