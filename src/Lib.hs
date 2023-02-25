module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
    putStrLn "a simple avatar maker -- work in progress!"
    putStrLn "Please pick a hair colour from the following: black, brown, honey, blonde"
    hairColour <- getAnswer ["black","brown","honey","blonde"]

    putStrLn "Please pick a hair length from the following: short, medium, long"
    hairLength <- getAnswer ["short","medium","long"] 

    putStrLn "Please pick a hair texture from the following: straight, wavy, curly"
    hairTexture <- getAnswer ["straight","wavy","curly"] 

    putStrLn "Please pick a skin colour from the following: light, tan, medium, dark"
    skinColour <- getAnswer ["light","tan","medium", "dark"] 

    putStrLn "And finally, please pick an eye colour from the following: blue, green, hazel, brown"
    eyeColour <- getAnswer ["blue","green","hazel", "brown"]
    
    putStrLn "now generating avatar"



getAnswer :: [String] -> IO String
getAnswer elemlist = do
    line <- getLine
    if line `elem` elemlist
       then return line
       else do
          putStrLn "Please choose one of the provided options!"
          getAnswer elemlist
