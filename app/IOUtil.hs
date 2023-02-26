module IOUtil (loadAssets, formAssetPaths, outputPath) where

import Codec.Picture

imagesPath :: String
imagesPath = "./images/"

outputPath :: String
outputPath = "./output/"

loadAssets :: [String] -> IO [(Image PixelRGBA8)]
loadAssets paths = mapM loadImage paths

loadImage :: FilePath -> IO (Image PixelRGBA8)
loadImage path = do
    result <- readImage path
    case result of
        Left errorMsg -> error $ "Error loading PNG image: " ++ errorMsg
        Right dynamicImage -> do
            let image :: Image PixelRGBA8
                image = convertRGBA8 dynamicImage
            return image

formAssetPaths :: String -> String -> String -> String -> String -> [String]
formAssetPaths currHairColour currHairLength currHairTexture currSkinColour currEyeColour = 
    reverse [formHairPath currHairLength currHairTexture currHairColour, formPath "eyes" currEyeColour, formPath "skin" currSkinColour]

formPath :: String -> String -> String
formPath part colour = imagesPath ++ part ++ " " ++ colour ++ ".png"

formHairPath :: String -> String -> String -> String
formHairPath hairLength hairTexture hairColour = formPath "hair" (hairLength ++ " " ++ hairTexture ++ " " ++ hairColour)