-- represents an asset loader. loads images from given file paths
-- includes methods specifically built to load Avatar part files
module AssetLoader (loadAssets, saveImage, outputPath, ImageType(..)) where

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

-- path to an error file, in case an error is encountered
-- these should be impossible to reach
errorPath :: String
errorPath = "error"

-- represents the type of image that needs to be extracted
data ImageType = Colour | Lines

-- determines the ending tag for an image's path
tag :: ImageType -> String
tag Colour = " colour.png"
tag Lines = " lines.png"

-- the body parts of an avatar
bodyParts :: [String]
bodyParts = ["skin", "shirt", "eyes"]


---------------
--- METHODS ---
---------------
-- loads the required avatar image assets, given the hair specifications
loadAssets :: [String] -> ImageType -> IO [Image PixelRGBA8]
loadAssets hairOptions imgtag = mapM loadImage (formAssetPaths hairOptions imgtag)

-- forms all the paths to the required image assets
formAssetPaths :: [String] -> ImageType -> [String]
formAssetPaths hairOptions imgtag = 
    let hairPaths = formHairPaths hairOptions imgtag
    in foldr (\x y -> formAvatarPartPaths x imgtag ++ y) hairPaths bodyParts

-- forms a path to a required image asset
formAvatarPartPaths :: String -> ImageType -> [String]
formAvatarPartPaths part imgtag = [imagesPath ++ part ++ tag imgtag]

-- specifically forms the required hair paths due to the uniqueness of the avatar part
formHairPaths :: [String] -> ImageType -> [String]
formHairPaths (h:m:t) imgtag = formAvatarPartPaths ("hair " ++ h ++ " " ++ m) imgtag ++ formBangsPaths t imgtag
formHairPaths _ imgtag = formErrorPath imgtag

-- specifically forms the required bangs path due to the uniqueness of the avatar part
formBangsPaths :: [String] -> ImageType -> [String]
formBangsPaths [t] imgtag = formAvatarPartPaths ("bangs " ++ t) imgtag
formBangsPaths _ imgtag = formErrorPath imgtag

-- sends a blank image if an error occurs
-- this should be impossible to reach
formErrorPath :: ImageType -> [String]
formErrorPath = formAvatarPartPaths errorPath

-- loads an image from a specified file path
loadImage :: FilePath -> IO (Image PixelRGBA8)
loadImage path = do
    result <- readImage path
    -- reference: https://stackoverflow.com/questions/30512442/juicypixels-cant-load-png-files
    case result of
        Left errorMsg -> error $ "Error loading PNG image: " ++ errorMsg
        Right dynamicImage -> do
            let image :: Image PixelRGBA8
                image = convertRGBA8 dynamicImage
            return image

saveImage :: FilePath -> Image PixelRGBA8 -> IO ()
saveImage path image = do
    writePng (outputPath ++ path) image