module AvatarDisplay (displayAvatar) where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Codec.Picture

displayAvatar :: Image PixelRGBA8 -> IO ()
displayAvatar avatar = do
  let picture = fromImageRGBA8 avatar
  display (InWindow "My Window" ((imageWidth avatar), (imageHeight avatar)) (0, 0)) white picture

-- TODO:
-- displayAvatar
-- given a file path, displays the image on a simple GUI