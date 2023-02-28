-- represents an asset loader. loads images from given file paths
-- includes methods specifically built to load Avatar part files
module AssetLoader (loadAssets, outputPath, ImageType(..)) where

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
loadAssets :: String -> String -> ImageType -> IO [Image PixelRGBA8]
loadAssets hairstyle bangs imgtag = mapM loadImage (formAssetPaths hairstyle bangs (tag imgtag))

-- forms all the paths to the required image assets
formAssetPaths :: String -> String -> String -> [String]
formAssetPaths hairstyle bangs imgtag = 
    let hairPaths = formHairPaths hairstyle bangs imgtag
    in foldr (\x y -> formAvatarPartPaths x imgtag ++ y) hairPaths bodyParts

-- forms a path to a required image asset
formAvatarPartPaths :: String -> String -> [String]
formAvatarPartPaths part imgtag = [imagesPath ++ part ++ imgtag]

-- specifically forms the required hair paths due to the uniqueness of the avatar part
formHairPaths :: String -> String -> String -> [String]
formHairPaths hairstyle bangs imgtag = formAvatarPartPaths ("hair " ++ hairstyle) imgtag ++ formAvatarPartPaths ("bangs " ++ bangs) imgtag   

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