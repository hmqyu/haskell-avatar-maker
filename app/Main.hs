module Main ( main ) where

---------------
--- IMPORTS- --
---------------
import AvatarMakerApp ( runAvatarMakerApp )


---------------
-- FUNCTIONS --
---------------
main :: IO ()
main = do
    putStrLn "-- a simple avatar maker --"
    runAvatarMakerApp