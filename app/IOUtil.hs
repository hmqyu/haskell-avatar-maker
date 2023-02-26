module IOUtil (loadAssets, formAssetPaths, outputPath, colourTag, linesTag) where

import Codec.Picture

imagesPath :: String
imagesPath = "./avatar-parts/"

outputPath :: String
outputPath = "./output/"

colourTag :: String
colourTag = " colour.png"

linesTag :: String
linesTag = " lines.png"

bodyParts :: [String]
bodyParts = ["skin", "shirt", "eyes"]

loadAssets :: String -> String -> String -> String -> IO [Image PixelRGBA8]
loadAssets currHairTexture currHairLength hasBangs tag = mapM loadImage (formAssetPaths currHairTexture currHairLength hasBangs tag)

formAssetPaths :: String -> String -> String -> String -> [String]
formAssetPaths currHairLength currHairTexture hasBangs tag = 
    let hairPaths = formHairPaths currHairLength currHairTexture hasBangs tag
    in foldr (\x y -> formAvatarPartPaths x tag ++ y) hairPaths bodyParts

formAvatarPartPaths :: String -> String -> [String]
formAvatarPartPaths part tag = [imagesPath ++ part ++ tag]

formHairPaths :: String -> String -> String -> String -> [String]
formHairPaths hairTexture hairLength hasBangs tag =
    let hairPaths = formAvatarPartPaths ("hair " ++ hairTexture ++ " " ++ hairLength) tag in
    if hasBangs == "yes" then hairPaths ++ formAvatarPartPaths "bangs" tag
    else hairPaths

loadImage :: FilePath -> IO (Image PixelRGBA8)
loadImage path = do
    result <- readImage path
    case result of
        Left errorMsg -> error $ "Error loading PNG image: " ++ errorMsg
        Right dynamicImage -> do
            let image :: Image PixelRGBA8
                image = convertRGBA8 dynamicImage
            return image