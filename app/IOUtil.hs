-- represents various util functions for the Main class
module IOUtil (loadAssets, formAssetPaths, outputPath, colourTag, linesTag) where

---------------
--- IMPORTS ---
---------------
import Codec.Picture


---------------
-- CONSTANTS --
---------------
-- folder containing images to assemble the avatar
imagesPath :: String
imagesPath = "./avatar-parts/"

-- folder where the completed avatar will be saved to
outputPath :: String
outputPath = "./output/"

-- ending tag for the flat colour image of an avatar part
colourTag :: String
colourTag = " colour.png"

-- ending tag for the lineart image of an avatar part
linesTag :: String
linesTag = " lines.png"

-- the body parts of an avatar
bodyParts :: [String]
bodyParts = ["skin", "shirt", "eyes"]


---------------
--- METHODS ---
---------------
-- loads the required avatar image assets, given the hair specifications
loadAssets :: String -> String -> String -> String -> IO [Image PixelRGBA8]
loadAssets currHairTexture currHairLength hasBangs tag = mapM loadImage (formAssetPaths currHairTexture currHairLength hasBangs tag)

-- forms all the paths to the required image assets
formAssetPaths :: String -> String -> String -> String -> [String]
formAssetPaths currHairLength currHairTexture hasBangs tag = 
    let hairPaths = formHairPaths currHairLength currHairTexture hasBangs tag
    in foldr (\x y -> formAvatarPartPaths x tag ++ y) hairPaths bodyParts

-- forms a single path to a required image asset
formAvatarPartPaths :: String -> String -> [String]
formAvatarPartPaths part tag = [imagesPath ++ part ++ tag]

-- specifically forms the required hair path due to the uniqueness of the avatar part
formHairPaths :: String -> String -> String -> String -> [String]
formHairPaths hairTexture hairLength hasBangs tag =
    let hairPaths = formAvatarPartPaths ("hair " ++ hairTexture ++ " " ++ hairLength) tag in
    if hasBangs == "yes" then hairPaths ++ formAvatarPartPaths "bangs" tag
    else hairPaths

-- loads an image from a specified file path
loadImage :: FilePath -> IO (Image PixelRGBA8)
loadImage path = do
    result <- readImage path
    case result of
        Left errorMsg -> error $ "Error loading PNG image: " ++ errorMsg
        Right dynamicImage -> do
            let image :: Image PixelRGBA8
                image = convertRGBA8 dynamicImage
            return image