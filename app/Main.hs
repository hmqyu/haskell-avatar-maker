module Main (main, getAnswer) where
    
import IOUtil (loadAssets, outputPath, colourTag, linesTag)
import AvatarMaker (AvatarPart(..), apOptions, createAvatar)
import AvatarDisplay (displayAvatar)
import Codec.Picture

main :: IO ()
main = do
    putStrLn "a simple avatar maker -- work in progress!"
    runAvatarMaker

runAvatarMaker :: IO ()
runAvatarMaker = do
    putStrLn "Please pick a hair colour from the following: black, light brown, dark brown, blonde"
    currHairColour <- getAnswer HairColour

    putStrLn "Please pick a hair length from the following: short, medium, long"
    currHairLength <- getAnswer HairLength

    putStrLn "Please pick a hair texture from the following: straight, wavy, curly"
    currHairTexture <- getAnswer HairTexture

    putStrLn "Should your avatar have bangs? yes or no"
    currBangs <- getAnswer HasBangs

    putStrLn "Please pick a skin colour from the following: light, tan, medium, dark"
    currSkinColour <- getAnswer SkinColour

    putStrLn "Please pick an eye colour from the following: blue, green, light brown, dark brown"
    currEyeColour <- getAnswer EyeColour

    putStrLn "Please pick a shirt colour from the following: blue, green, red, black, white"
    currShirtColour <- getAnswer ShirtColour

    putStrLn "And finally, please give your character a name:"
    currName <- getLine

    putStrLn "now generating avatar..."

    avatarPartsImagesColoured <- loadAssets currHairTexture currHairLength currBangs colourTag
    avatarPartsImagesLineart <- loadAssets currHairTexture currHairLength currBangs linesTag
    let coloursSoFar = [currSkinColour, currShirtColour, currEyeColour, currHairColour]
    let avatarPartsColours = if currBangs == "yes" then coloursSoFar ++ [currHairColour] else coloursSoFar
        
    let avatar = createAvatar avatarPartsImagesColoured avatarPartsImagesLineart avatarPartsColours
    writePng (outputPath ++ currName ++ ".png") avatar
    -- displayAvatar avatar
    putStrLn "success! your avatar can now be found in the output folder."
    putStrLn "would you like to create another avatar?"
    putStrLn "type yes to continue, or any character to quit"
    decision <- getLine
    if decision == "yes" then runAvatarMaker
    else putStrLn "thanks for playing!"

getAnswer :: AvatarPart -> IO String
getAnswer elemlist = do
    line <- getLine
    if line `elem` apOptions elemlist
        then return line
        else do
            putStrLn "Please choose one of the provided options!"
            getAnswer elemlist