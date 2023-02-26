module Main (main, getAnswer) where
    
import Codec.Picture
-- import Control.Monad
import AvatarMaker (hairColours, hairLengths, hairTextures, skinColours, eyeColours, createAvatar)

-- constants
imagesPath :: String
imagesPath = "./images/"

outputPath :: String
outputPath = "./output/"

main :: IO ()
main = do
    putStrLn "a simple avatar maker -- work in progress!"
    putStrLn "Please pick a hair colour from the following: black, brown, honey, blonde"
    currHairColour <- getAnswer hairColours

    putStrLn "Please pick a hair length from the following: short, medium, long"
    currHairLength <- getAnswer hairLengths

    putStrLn "Please pick a hair texture from the following: straight, wavy, curly"
    currHairTexture <- getAnswer hairTextures

    putStrLn "Please pick a skin colour from the following: light, tan, medium, dark"
    currSkinColour <- getAnswer skinColours

    putStrLn "And finally, please pick an eye colour from the following: blue, green, hazel, brown"
    currEyeColour <- getAnswer eyeColours

    putStrLn "And finally, please give your character a name:"
    currName <- getLine

    putStrLn "now generating avatar"

    images <- loadAssets (formAssetPaths currHairColour currHairLength currHairTexture currSkinColour currEyeColour)

    writePng (outputPath ++ currName ++ ".png") (createAvatar images)
    putStrLn "Success!"

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

getAnswer :: [String] -> IO String
getAnswer elemlist = do
    line <- getLine
    if line `elem` elemlist
       then return line
       else do
          putStrLn "Please choose one of the provided options!"
          getAnswer elemlist


