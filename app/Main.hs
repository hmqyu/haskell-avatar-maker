module Main (main, getAnswer) where
    
import IOUtil (loadAssets, formAssetPaths, outputPath)
import AvatarMaker (hairColours, hairLengths, hairTextures, skinColours, eyeColours, createAvatar)
import AvatarDisplay (displayAvatar)
import Codec.Picture

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

    putStrLn "And finally, please give your character a name:"
    currName <- getLine

    putStrLn "now generating avatar..."

    images <- loadAssets (formAssetPaths currHairColour currHairLength currHairTexture currSkinColour currEyeColour)
    let avatar = createAvatar images
    writePng (outputPath ++ currName ++ ".png") avatar
    displayAvatar(avatar)
    putStrLn "Success!"

getAnswer :: [String] -> IO String
getAnswer elemlist = do
    line <- getLine
    if line `elem` elemlist
       then return line
       else do
          putStrLn "Please choose one of the provided options!"
          getAnswer elemlist