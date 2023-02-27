module Main (main, checkAnswer) where
    
import IOUtil (loadAssets, outputPath, colourTag, linesTag)
import AvatarMaker (AvatarPart(..), avatar, createAvatar)
import Colours (colourToRGBA8)
-- import AvatarDisplay (displayAvatar)
import Codec.Picture

main :: IO ()
main = do
    putStrLn "-- a simple avatar maker --"
    runAvatarMaker

runAvatarMaker :: IO ()
runAvatarMaker = do
    putStrLn "please pick a hair colour from the following: black, light brown, dark brown, blonde"
    currHairColour <- checkAnswer HairColour

    putStrLn "please pick a hair length from the following: short, medium, long"
    currHairLength <- checkAnswer HairLength

    putStrLn "please pick a hair texture from the following: straight, wavy, curly"
    currHairTexture <- checkAnswer HairTexture

    putStrLn "should your avatar have bangs? yes or no"
    currBangs <- checkAnswer HasBangs

    putStrLn "please pick a skin colour from the following: light, tan, medium, dark"
    currSkinColour <- checkAnswer SkinColour

    putStrLn "please pick an eye colour from the following: blue, green, light brown, dark brown"
    currEyeColour <- checkAnswer EyeColour

    putStrLn "please pick a shirt colour from the following: blue, green, red, black, white"
    currShirtColour <- checkAnswer ShirtColour

    putStrLn "and finally, please give your character a name:"
    currName <- getLine

    putStrLn "now generating avatar..."

    avatarPartsImagesColoured <- loadAssets currHairTexture currHairLength currBangs colourTag
    avatarPartsImagesLineart <- loadAssets currHairTexture currHairLength currBangs linesTag
    let coloursSoFar = [colourToRGBA8 currSkinColour, colourToRGBA8 currShirtColour, colourToRGBA8 currEyeColour, colourToRGBA8 currHairColour]
    let avatarPartsColours = if currBangs == "yes" then coloursSoFar ++ [colourToRGBA8 currHairColour] else coloursSoFar
        
    let currAvatar = createAvatar avatarPartsImagesColoured avatarPartsImagesLineart avatarPartsColours
    writePng (outputPath ++ currName ++ ".png") currAvatar
    putStrLn "success! your avatar can now be found in the output folder."
    putStrLn "would you like to create another avatar?"
    putStrLn "type yes to continue, or any character to quit"
    decision <- getLine
    if decision == "yes" then runAvatarMaker
    else putStrLn "thanks for playing!"

-- designAvatar :: IO [String]
-- designAvatar = mapM askQuestion

checkAnswer :: AvatarPart -> IO String
checkAnswer elemlist = do
    line <- getLine
    if line `elem` avatar elemlist
        then return line
        else do
            putStrLn "Please choose one of the provided options!"
            checkAnswer elemlist