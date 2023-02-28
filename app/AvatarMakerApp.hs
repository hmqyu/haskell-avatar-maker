-- 
module AvatarMakerApp ( runAvatarMakerApp ) where

---------------
--- IMPORTS- --
---------------
import AssetLoader ( loadAssets, outputPath, ImageType(..) )
import AvatarCreator ( Avatar(..), avatarToStrings, createAvatar )
import Codec.Picture


---------------
-- CONSTANTS --
---------------
questionBegin :: String
questionBegin = "select a "

questionEnd :: String
questionEnd = " from the following options: "


---------------
-- FUNCTIONS --
---------------
-- runs the app that allows the user to build an avatar however many times they want
-- avatars are saved as PNGs in the designated output folder
runAvatarMakerApp :: IO ()
runAvatarMakerApp = do
    currHairLength <- findUserOptionFor HairLength
    currHairTexture <- findUserOptionFor HairTexture
    currBangs <- findUserOptionFor BangsStyle
    currHairColour <- findUserOptionFor HairColour
    currSkinColour <- findUserOptionFor SkinColour
    currEyeColour <- findUserOptionFor EyeColour
    currShirtColour <- findUserOptionFor ShirtColour

    putStrLn "and finally, please give your character a name:"
    currName <- getLine

    putStrLn "now generating avatar..."
    
    let hairstyle = currHairTexture ++ " " ++ currHairLength
    imageColours <- loadAssets hairstyle currBangs Colour
    imageLineart <- loadAssets hairstyle currBangs Lines
    let coloursSoFar = [currSkinColour, currShirtColour, currEyeColour, currHairColour]
    let selectedColours = if not (currBangs == "none") then coloursSoFar ++ [currHairColour] else coloursSoFar
        
    let currAvatar = createAvatar imageColours imageLineart selectedColours
    writePng (outputPath ++ currName ++ ".png") currAvatar
    putStrLn "success! your avatar can now be found in the output folder."
    putStrLn "would you like to create another avatar?"
    putStrLn "type yes to continue, or any character to quit"
    decision <- getLine
    if decision == "yes" then runAvatarMakerApp
    else putStrLn "thanks for playing!"

-- determines the user's selected option for an avatar part
findUserOptionFor :: Avatar -> IO String
findUserOptionFor op = do
    let part = fst (avatarToStrings op)
    let options = snd (avatarToStrings op)
    askQuestion part options
    checkAnswer options

-- asks a question to the user, presents the answers, validates their answer, then returns it
askQuestion :: String -> [String] -> IO ()
askQuestion _ [] = do putStrLn "An error has occured." -- impossible to reach this line as options always has something in it
askQuestion part (h:t) = do
    let question = questionBegin ++ part ++ questionEnd
    let options = foldl (\x y -> x ++ ", " ++ y) h t
    putStrLn (question ++ options)

-- checks to make sure that the user inputted a valid answer according to the given list of options
checkAnswer :: [String] -> IO String
checkAnswer elemlist = do
    line <- getLine
    if line `elem` elemlist
        then return line
        else do
            putStrLn "please choose one of the provided options!"
            checkAnswer elemlist