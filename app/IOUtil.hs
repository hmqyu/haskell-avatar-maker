module IOUtil (loadAssets, formAssetPaths, outputPath) where

import Codec.Picture
import AvatarMaker (avatarParts)

imagesPath :: String
imagesPath = "./images2/"

outputPath :: String
outputPath = "./output/"

loadAssets :: String -> String -> String -> IO [Image PixelRGBA8]
loadAssets currHairLength currHairTexture hasBangs = mapM loadImage (formAssetPaths currHairLength currHairTexture hasBangs)

formAssetPaths :: String -> String -> String -> [String]
formAssetPaths currHairLength currHairTexture hasBangs = 
    foldr (\x y -> formAvatarPartPaths x ++ y) hairPaths avatarParts
    where hairPaths = formHairPaths currHairLength currHairTexture hasBangs

formAvatarPartPaths :: String -> [String]
formAvatarPartPaths part = [imagesPath ++ part ++ " colour.png", imagesPath ++ part ++ " lines.png"]

formHairPaths :: String -> String -> String -> [String]
formHairPaths hairTexture hairLength hasBangs
    | hasBangs == "yes" = hairPaths ++ formAvatarPartPaths "bangs"
    | otherwise = hairPaths
    where hairPaths = formAvatarPartPaths ("hair " ++ hairTexture ++ " " ++ hairLength)

loadImage :: FilePath -> IO (Image PixelRGBA8)
loadImage path = do
    result <- readImage path
    case result of
        Left errorMsg -> error $ "Error loading PNG image: " ++ errorMsg
        Right dynamicImage -> do
            let image :: Image PixelRGBA8
                image = convertRGBA8 dynamicImage
            return image

-- formAssets :: String -> String -> String -> String -> String -> String -> String -> IO [Image PixelRGBA8]
-- formAssets hairColour hairLength hairTexture skinColour eyeColour shirtColour hasBangs = do
--     eyeImage <- mergeEye eyeColour
--     hairImage <- mergeHair hairColour hairLength hairTexture
--     baseImage <- mergeBase skinColour shirtColour
--     bangsImage <- if hasBangs == "yes" then mergeBangs hairColour else loadImage(formPath "blank")
--     return [baseImage, hairImage, bangsImage, eyeImage]


-- mergeBangs :: String -> IO(Image PixelRGBA8)
-- mergeBangs hairColour = do
--     lineImage <- loadImage (formPath "bangs lines")
--     colourImage <-  loadImage (formPath "bangs colour")
    
--     let colourImageNew = dyeImage colourImage (colourToRGBA8 hairColour)
--     return (mergeImages lineImage colourImageNew)


-- mergeEye :: String -> IO(Image PixelRGBA8)
-- mergeEye eyeColour = do
--     lineImage <- loadImage (formPath "eyes lines")
--     colourImage <-  loadImage (formPath "eyes colour")
    
--     let colourImageNew = dyeImage colourImage (colourToRGBA8 eyeColour)
--     return (mergeImages lineImage colourImageNew)


-- mergeBase :: String -> String -> IO(Image PixelRGBA8)
-- mergeBase skinColour shirtColour = do
--     baseLineImage <- loadImage (formPath "base lines")
--     baseColourImage <- loadImage (formPath "base colour")
--     shirtColourImage <- loadImage (formPath "shirt colour")

--     let 
--         baseColourImageNew = dyeImage baseColourImage (colourToRGBA8 skinColour)
--         shirtColourImageNew = dyeImage shirtColourImage (colourToRGBA8 shirtColour)
--     return (mergeImages baseLineImage (mergeImages baseColourImageNew shirtColourImageNew))



-- mergeHair :: String -> String -> String -> IO(Image PixelRGBA8)
-- mergeHair hairColour hairLength hairTexture = do
--     colourImage <- loadImage (formPath ("hair " ++ hairTexture ++ " " ++ hairLength ++ " colour"))
--     lineImage <- loadImage (formPath ("hair " ++ hairTexture ++ " " ++ hairLength ++ " lines"))

--     let colourImageNew = dyeImage colourImage (colourToRGBA8 hairColour)
--     return (mergeImages lineImage colourImageNew)
    