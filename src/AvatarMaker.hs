module AvatarMaker
    ( hairColours, hairLengths, hairTextures, skinColours, eyeColours ) where

---------------
-- CONSTANTS --
---------------

hairColours :: [String]
hairColours = ["black", "brown", "honey", "blonde"]

hairLengths :: [String]
hairLengths = ["short","medium","long"] 

hairTextures :: [String]
hairTextures = ["straight","wavy","curly"] 

skinColours :: [String]
skinColours = ["light","tan","medium", "dark"] 

eyeColours :: [String]
eyeColours = ["blue","green","hazel", "brown"]

----------------
---- FIELDS ----
----------------


---------------
--- METHODS ---
---------------

-- TODO:
-- createAvatar 
-- creates the avatar using the given components

-- TODO:
-- mergeImages
-- merges two PNG images together into one PNG image
-- the first image passed will be merged on top of the second image passed

-- TODO:
-- dyeImage
-- dyes an image either using a defined colour constant or a given RGB/hexcode value