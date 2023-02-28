module AvatarMakerApp ( runAvatarMakerApp ) where

---------------
--- IMPORTS- --
---------------
import AssetLoader ( loadAssets, outputPath, ImageType(..) )
import AvatarCreator ( AvatarPart(..), createAvatar, avatarToString )
import Colours ( colourToRGBA8 )
-- import AvatarDisplay (displayAvatar)
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
    currHairLength <- getSelectedPart HairLength
    currHairTexture <- getSelectedPart HairTexture
    currBangs <- getSelectedPart HasBangs
    currHairColour <- getSelectedPart HairColour
    currSkinColour <- getSelectedPart SkinColour
    currEyeColour <- getSelectedPart EyeColour
    currShirtColour <- getSelectedPart ShirtColour

    putStrLn "and finally, please give your character a name:"
    currName <- getLine

    putStrLn "now generating avatar..."

    imageColours <- loadAssets currHairTexture currHairLength currBangs Colour
    imageLineart <- loadAssets currHairTexture currHairLength currBangs Lines
    let coloursSoFar = [colourToRGBA8 currSkinColour, colourToRGBA8 currShirtColour, colourToRGBA8 currEyeColour, colourToRGBA8 currHairColour]
    let selectedColours = if currBangs == "yes" then coloursSoFar ++ [colourToRGBA8 currHairColour] else coloursSoFar
        
    let currAvatar = createAvatar imageColours imageLineart selectedColours
    writePng (outputPath ++ currName ++ ".png") currAvatar
    putStrLn "success! your avatar can now be found in the output folder."
    putStrLn "would you like to create another avatar?"
    putStrLn "type yes to continue, or any character to quit"
    decision <- getLine
    if decision == "yes" then runAvatarMakerApp
    else putStrLn "thanks for playing!"

-- build an avatar given the user's inputs
-- buildAvatar :: IO [String]

-- determines the user's selected option for an avatar part
getSelectedPart :: AvatarPart -> IO String
getSelectedPart ap = do
    let part = fst (avatarToString ap)
    let options = snd (avatarToString ap)
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