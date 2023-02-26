module Main (main, getAnswer) where
    
import Codec.Picture
-- import Control.Monad
import AvatarMaker (hairColours, hairLengths, hairTextures, skinColours, eyeColours, shirtColours, yesNo, createAvatar, mergeImages, dyeImage, colourToRGBA8)

-- constants
imagesPath :: String
imagesPath = "./images2/"

outputPath :: String
outputPath = "./output/"

main :: IO ()
main = do
    putStrLn "a simple avatar maker -- work in progress!"
    putStrLn "Please pick a hair colour from the following: black, light brown, dark brown, blonde"
    currHairColour <- getAnswer hairColours

    putStrLn "Please pick a hair length from the following: short, medium, long"
    currHairLength <- getAnswer hairLengths

    putStrLn "Please pick a hair texture from the following: straight, wavy, curly"
    currHairTexture <- getAnswer hairTextures

    putStrLn "Should your avatar have bangs? yes or no"
    hasBangs <- getAnswer yesNo

    putStrLn "Please pick a skin colour from the following: light, tan, medium, dark"
    currSkinColour <- getAnswer skinColours

    putStrLn "Please pick an eye colour from the following: blue, green, light brown, dark brown"
    currEyeColour <- getAnswer eyeColours

    putStrLn "Please pick a shirt colour from the following: blue, green, red, black, white"
    currShirtColour <- getAnswer shirtColours

    putStrLn "And finally, please give your character a name:"
    currName <- getLine

    putStrLn "now generating avatar"

    images <- formAssets currHairColour currHairLength currHairTexture currSkinColour currEyeColour currShirtColour hasBangs

    writePng (outputPath ++ currName ++ ".png") (createAvatar images)
    putStrLn "Success!"

formAssets :: String -> String -> String -> String -> String -> String -> String -> IO [Image PixelRGBA8]
formAssets hairColour hairLength hairTexture skinColour eyeColour shirtColour hasBangs = do
    eyeImage <- mergeEye eyeColour
    hairImage <- mergeHair hairColour hairLength hairTexture
    baseImage <- mergeBase skinColour shirtColour
    bangsImage <- if hasBangs == "yes" then mergeBangs hairColour else loadImage(formPath "blank")
    return [baseImage, hairImage, bangsImage, eyeImage]


mergeBangs :: String -> IO(Image PixelRGBA8)
mergeBangs hairColour = do
    lineImage <- loadImage (formPath "bangs lines")
    colourImage <-  loadImage (formPath "bangs colour")
    
    let colourImageNew = dyeImage colourImage (colourToRGBA8 hairColour)
    return (mergeImages lineImage colourImageNew)


mergeEye :: String -> IO(Image PixelRGBA8)
mergeEye eyeColour = do
    lineImage <- loadImage (formPath "eyes lines")
    colourImage <-  loadImage (formPath "eyes colour")
    
    let colourImageNew = dyeImage colourImage (colourToRGBA8 eyeColour)
    return (mergeImages lineImage colourImageNew)


mergeBase :: String -> String -> IO(Image PixelRGBA8)
mergeBase skinColour shirtColour = do
    baseLineImage <- loadImage (formPath "base lines")
    baseColourImage <- loadImage (formPath "base colour")
    shirtColourImage <- loadImage (formPath "shirt colour")

    let 
        baseColourImageNew = dyeImage baseColourImage (colourToRGBA8 skinColour)
        shirtColourImageNew = dyeImage shirtColourImage (colourToRGBA8 shirtColour)
    return (mergeImages baseLineImage (mergeImages baseColourImageNew shirtColourImageNew))



mergeHair :: String -> String -> String -> IO(Image PixelRGBA8)
mergeHair hairColour hairLength hairTexture = do
    colourImage <- loadImage (formPath ("hair " ++ hairTexture ++ " " ++ hairLength ++ " colour"))
    lineImage <- loadImage (formPath ("hair " ++ hairTexture ++ " " ++ hairLength ++ " lines"))

    let colourImageNew = dyeImage colourImage (colourToRGBA8 hairColour)
    return (mergeImages lineImage colourImageNew)
    
    



--loadAssets :: [String] -> IO [(Image PixelRGBA8)]
--loadAssets paths = mapM loadImage paths


loadImage :: FilePath -> IO (Image PixelRGBA8)
loadImage path = do
    result <- readImage path
    case result of
        Left errorMsg -> error $ "Error loading PNG image: " ++ errorMsg
        Right dynamicImage -> do
            let image :: Image PixelRGBA8
                image = convertRGBA8 dynamicImage
            return image


--formAssetPaths :: String -> String -> String -> String -> String -> [String]
--formAssetPaths currHairColour currHairLength currHairTexture currSkinColour currEyeColour = 
--    reverse [formHairPath currHairLength currHairTexture, formPath "eyes", formPath "base"]

formPath :: String -> String
formPath part = imagesPath ++ part ++ ".png"

--formHairPath :: String -> String -> String
--formHairPath hairLength hairTexture = formPath ("hair " ++ hairTexture ++ " " ++ hairLength )

getAnswer :: [String] -> IO String
getAnswer elemlist = do
    line <- getLine
    if line `elem` elemlist
       then return line
       else do
          putStrLn "Please choose one of the provided options!"
          getAnswer elemlist


