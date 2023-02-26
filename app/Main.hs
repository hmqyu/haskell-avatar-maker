module Main (main, getAnswer) where
    
import Codec.Picture
import AvatarMaker (hairColours, hairLengths, hairTextures, skinColours, eyeColours, copyPixels)

-- constants

main :: IO ()
main = do
    putStrLn "a simple avatar maker -- work in progress!"
    putStrLn "Please pick a hair colour from the following: black, light brown, dark brown, blonde"
    currHairColour <- getAnswer hairColours

    putStrLn "Please pick a hair length from the following: short, medium, long"
    currHairLength <- getAnswer hairLengths

    putStrLn "Please pick a hair texture from the following: straight, wavy, curly"
    currHairTexture <- getAnswer hairTextures

    putStrLn "Please pick a skin colour from the following: light, tan, medium, dark"
    currSkinColour <- getAnswer skinColours

    putStrLn "And finally, please pick an eye colour from the following: blue, green, light brown, dark brown"
    currEyeColour <- getAnswer eyeColours

    putStrLn "now generating avatar"

    image1 <- loadImage "./images/bangs blonde.png"
    image2 <- loadImage "./images/skin tan.png"
    writePng "merged.png" (copyPixels image1 image2)
    putStrLn "Success!"

loadImage :: FilePath -> IO (Image PixelRGBA8)
loadImage path = do
    result <- readImage path
    case result of
        Left errorMsg -> do
            let image :: Image PixelRGBA8
                image = generateImage (\x y -> PixelRGBA8 0 0 0 0) 600 600
            putStrLn $ "An error occurred"
            -- fix this to return an error or smth
            return image
        Right dynamicImage -> do
            let image :: Image PixelRGBA8
                image = convertRGBA8 dynamicImage
            return image

getAnswer :: [String] -> IO String
getAnswer elemlist = do
    line <- getLine
    if line `elem` elemlist
       then return line
       else do
          putStrLn "Please choose one of the provided options!"
          getAnswer elemlist


