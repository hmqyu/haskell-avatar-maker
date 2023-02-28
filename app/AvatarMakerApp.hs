-- 
module AvatarMakerApp ( runAvatarMakerApp ) where

---------------
--- IMPORTS- --
---------------
import AssetLoader ( loadAssets, saveImage, ImageType(..) )
import AvatarCreator ( Avatar(..), avatarOptions, avatarColours, avatarToStrings, createAvatar )


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
    selectedAvatarOptions <- getAllUserOptionsFor avatarOptions
    selectedAvatarColours <- getAllUserOptionsFor avatarColours

    putStrLn "and finally, please give your character a name:"
    name <- getLine

    putStrLn "now generating avatar..."
    
    imageColours <- loadAssets selectedAvatarOptions Colour
    imageLineart <- loadAssets selectedAvatarOptions Lines
    let currAvatar = createAvatar imageColours imageLineart (reverse selectedAvatarColours)
    saveImage (name ++ ".png") currAvatar

    putStrLn "success! your avatar can now be found in the designated folder."
    putStrLn "would you like to create another avatar?"
    putStrLn "type yes to continue, or any character to quit"
    decision <- getLine
    if decision == "yes" then runAvatarMakerApp
    else putStrLn "thanks for playing!"

-- gets all user selections for a given set of avatar colours/options
getAllUserOptionsFor :: [Avatar] -> IO [String]
getAllUserOptionsFor optionsList = do
    let n = length optionsList
        findUserOptionForQuestionAtGivenIndex i = do
            findUserOptionFor (optionsList !! i)
    mapM findUserOptionForQuestionAtGivenIndex [0..n-1]

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