module ImageMerger ( mergeAllImages ) where

---------------
--- IMPORTS ---
---------------
import Codec.Picture


---------------
-- FUNCTIONS --
---------------
-- flattens all images into a single image from left to right
-- the first image in the array is the base
mergeAllImages :: [Image PixelRGBA8] -> Image PixelRGBA8
-- base case impossible to reach in Main.hs, but added regardless; generates a 1x1 empty image
mergeAllImages [] = generateImage (\x y -> PixelRGBA8 (fromIntegral x) (fromIntegral y) 0 0) 1 1
mergeAllImages (h:t) = foldl (\x y -> mergeImages y x) h t

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

-- deals with pixels that have transparency (alpha < 1) 
-- determines what colour a pixel should be if a pixel with transparency is layered on top
-- the first pixel passed is layered on top of the second pixel passed
mixPixel :: PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
mixPixel (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
    if a2 == 0 then PixelRGBA8 r1 g1 b1 a1
    else PixelRGBA8
        (mixPixelValue r1 r2 a1)
        (mixPixelValue g1 g2 a1)
        (mixPixelValue b1 b2 a1)
        (mixPixelValue a1 a2 a1)

-- determines the colour value if a colour with transparency is layered on top
mixPixelValue :: (Integral a) => a -> a -> a -> a
mixPixelValue c1 c2 alpha = round ((fromIntegral alpha / 255.0) * fromIntegral c1 + (fromIntegral (255 - alpha) / 255.0) * fromIntegral c2)