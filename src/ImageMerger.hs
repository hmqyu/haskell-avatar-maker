-- represents a set of functions used to merge multiple images together
-- merging takes into account the transparency of the image(s)
module ImageMerger ( mergeAllImages ) where

---------------
--- IMPORTS ---
---------------
import Codec.Picture


---------------
-- CONSTANTS --
---------------
-- represents the base image width and height
imgWidth :: Int
imgWidth = 896
imgHeight :: Int
imgHeight = 1046


---------------
-- FUNCTIONS --
---------------
-- flattens all images into a single image from left to right
-- assumes images in the list are all the same size
-- the base image is an empty image
mergeAllImages :: [Image PixelRGBA8] -> Image PixelRGBA8
mergeAllImages = mergeImageWithList base where
    mergeImageWithList img [] = img
    mergeImageWithList img (h:t) = mergeImageWithList (mergeImages h img) t
    base = generateImage (\x y -> PixelRGBA8 (fromIntegral x) (fromIntegral y) 0 0) imgWidth imgHeight

-- merges two PNG images together into one PNG image
-- the first image passed will be merged on top of the second image passed
mergeImages :: Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8
mergeImages topImage bottomImage = generateImage mergePixel width height
  where
    mergePixel x y 
        | pixelOpacity (pixelAt topImage x y) == 0 = pixelAt bottomImage x y
        | (pixelOpacity (pixelAt bottomImage x y) == 0) || (pixelOpacity (pixelAt topImage x y) == 255) = pixelAt topImage x y
        | otherwise = findPixelRGBA8 (pixelAt topImage x y) (pixelAt bottomImage x y)
    width = imageWidth topImage
    height = imageHeight topImage

-- deals with pixels that have transparency (alpha < 1) 
-- determines what colour a pixel should be if a pixel with transparency is layered on top
-- the first pixel passed is layered on top of the second pixel passed
findPixelRGBA8 :: PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
findPixelRGBA8 (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
    if a2 == 0 then PixelRGBA8 r1 g1 b1 a1
    else PixelRGBA8
        (findColourChannelValue r1 r2 a1)
        (findColourChannelValue g1 g2 a1)
        (findColourChannelValue b1 b2 a1)
        255 
-- set alpha = 255 because it's impossible for mixPixel to be called on an a2 < 255 in our program
-- a2 must be 255 here, or a combination of a1 and a2 results in 255

-- determines the colour value if a colour with transparency is layered on top
findColourChannelValue :: (Integral a) => a -> a -> a -> a
findColourChannelValue ccv1 ccv2 alpha = round ((fromIntegral alpha / 255.0) * fromIntegral ccv1 + (fromIntegral (255 - alpha) / 255.0) * fromIntegral ccv2)