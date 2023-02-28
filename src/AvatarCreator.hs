-- represents an avatar creator that creates an image of an avatar
-- given the avatar assets and colours
module AvatarCreator ( Avatar(..), avatarOptions, avatarColours, avatarToStrings, createAvatar ) where

---------------
--- IMPORTS- --
---------------
import Colours ( colourToRGBA8, dyeAllImages )
import ImageMerger ( mergeAllImages )
import Codec.Picture ( Image, PixelRGBA8 )


---------------
-- CONSTANTS --
---------------
-- represents the different parts of an avatar
data Avatar = HairTexture | HairLength | BangsStyle 
            | HairColour | BangsColour| EyeColour | SkinColour | ShirtColour

-- represents the different options for the avatar parts
avatarOptions :: [Avatar]
avatarOptions = [HairTexture, HairLength, BangsStyle]

-- represents the colours of each avatar part
avatarColours :: [Avatar]
avatarColours = [BangsColour, HairColour, EyeColour, ShirtColour, SkinColour]

-- converts Option into a string version, with possible options for the part
avatarToStrings :: Avatar -> (String, [String])
avatarToStrings HairTexture = ("hair texture", ["straight","wavy","curly"])
avatarToStrings HairLength = ("hair length", ["short","medium","long"])
avatarToStrings BangsStyle = ("bangs style", ["straight","none"])
avatarToStrings HairColour = ("hair colour", ["black brown", "chocolate brown", "light brown", "blonde", "red"])
avatarToStrings BangsColour = ("bangs colour", ["black brown", "chocolate brown", "light brown", "blonde", "red"])
avatarToStrings EyeColour = ("eye colour", ["blue","green","light brown", "chocolate brown"])
avatarToStrings ShirtColour = ("shirt colour", ["blue","green","red","black", "white"])
avatarToStrings SkinColour = ("skin colour", ["pale","light","tan","brown","dark brown","black"])


---------------
--- METHODS ---
---------------
-- creates the finished avatar PNG using the given components
createAvatar :: [Image PixelRGBA8] -> [Image PixelRGBA8] -> [String] -> Image PixelRGBA8
createAvatar flats linearts colours = mergeAllImages (weaveLayers (dyeAllImages flats (stringToPixelRGBA8 colours)) linearts)

-- weaves the flat colour image in between the lineart images
-- this is done so that layers are merged down correctly (lineart on top of flat colours)
weaveLayers :: [Image PixelRGBA8] -> [Image PixelRGBA8] -> [Image PixelRGBA8]
weaveLayers flats linearts = foldr (\(flat, lineart) y -> [flat, lineart] ++ y) [] (zip flats linearts)

-- converts the given list of colours into their PixelRGBA8 equivalent
stringToPixelRGBA8 :: [String] -> [PixelRGBA8]
stringToPixelRGBA8 colours = [colourToRGBA8 x | x <- colours]