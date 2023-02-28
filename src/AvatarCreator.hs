module AvatarCreator ( AvatarPart(..), avatar, avatarToString, createAvatar ) where

---------------
--- IMPORTS- --
---------------
import Colours ( dyeAllImages )
import ImageMerger ( mergeAllImages )
import Codec.Picture ( Image, PixelRGBA8 )


---------------
-- CONSTANTS --
---------------
-- represents all the different parts of an avatar
data AvatarPart = HairTexture | HairLength | HasBangs 
                | HairColour | EyeColour | SkinColour | ShirtColour

-- all avatar part components, split up into part types and part colours
avatar :: ([AvatarPart], [AvatarPart])
avatar = ([HairTexture, HairLength, HasBangs], [HairColour, EyeColour, SkinColour, ShirtColour])

-- converts AvatarPart into a string version, with possible options for the part
avatarToString :: AvatarPart -> (String, [String])
avatarToString HairTexture = ("hair texture", ["straight","wavy","curly"])
avatarToString HairLength = ("hair length", ["short","medium","long"])
avatarToString HasBangs = ("bangs", ["yes","no"])
avatarToString HairColour = ("hair colour", ["black", "light brown", "dark brown", "blonde"])
avatarToString EyeColour = ("eye colour", ["blue","green","light brown", "dark brown"])
avatarToString SkinColour = ("skin colour", ["light","tan","medium", "dark"])
avatarToString ShirtColour = ("shirt colour", ["blue","green","red","black", "white"])


---------------
--- METHODS ---
---------------
-- creates the finished avatar PNG using the given components
createAvatar :: [Image PixelRGBA8] -> [Image PixelRGBA8] -> [PixelRGBA8] -> Image PixelRGBA8
createAvatar flats linearts colours = mergeAllImages (weaveLayers (dyeAllImages flats colours) linearts)

-- weaves the flat colour image in between the lineart images
-- this is done so that layers are merged down correctly (lineart on top of flat colours)
weaveLayers :: [Image PixelRGBA8] -> [Image PixelRGBA8] -> [Image PixelRGBA8]
weaveLayers flats linearts = foldr (\(flat, lineart) y -> [flat, lineart] ++ y) [] (zip flats linearts)