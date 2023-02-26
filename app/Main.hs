module Main (main, getAnswer) where
    
import IOUtil (loadAssets, outputPath)
import AvatarMaker (hairColours, hairLengths, hairTextures, skinColours, eyeColours, shirtColours, yesNo, createAvatar)
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

    putStrLn "now generating avatar..."

    avatarPartsImages <- loadAssets currHairLength currHairTexture hasBangs
    let coloursSoFar = [currShirtColour, currSkinColour, currEyeColour, currHairColour]
    let avatarPartsColours = if hasBangs == "yes" then coloursSoFar ++ [currHairColour] else coloursSoFar
        
    let avatar = createAvatar avatarPartsImages avatarPartsColours
    writePng (outputPath ++ currName ++ ".png") avatar
    displayAvatar avatar
    putStrLn "Success!"

getAnswer :: [String] -> IO String
getAnswer elemlist = do
    line <- getLine
    if line `elem` elemlist
        then return line
        else do
            putStrLn "Please choose one of the provided options!"
            getAnswer elemlist