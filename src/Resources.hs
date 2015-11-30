{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Resources where

import Codec.Picture
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Map
import Data.Maybe
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import qualified Data.Vector.Storable as V
import System.FilePath
import Init.OpenGL

data Texture = Bind GLuint

data Resources = Resources
               { _textures :: Map String Texture }
makeLenses ''Resources

mkResources :: Resources
mkResources = Resources empty

toTexture :: GLuint -> Texture
toTexture = Bind

fromTexture :: Texture -> GLuint
fromTexture (Bind h) = h

unsafeImageData :: MonadIO m => DynamicImage -> m ()
unsafeImageData di = liftIO $ case di of
    ImageRGB8   i -> apply i GL_RGB  GL_RGB  GL_UNSIGNED_BYTE
    ImageRGB16  i -> apply i GL_RGB  GL_RGB  GL_UNSIGNED_SHORT
    ImageRGBA8  i -> apply i GL_RGBA GL_RGBA GL_UNSIGNED_BYTE
    ImageRGBA16 i -> apply i GL_RGBA GL_RGBA GL_UNSIGNED_SHORT
    _ -> error "uwap was too lazy"
  where
    {-# INLINE apply #-}
    apply i internalformat format size = V.unsafeWith (imageData i) $ \ptr ->
      let uptr   = castPtr ptr
          width  = fromIntegral (imageWidth i)
          height = fromIntegral (imageHeight i)
      in glTexImage2D GL_TEXTURE_2D 0 internalformat width height 0 format size uptr

loadPng :: MonadIO m => FilePath -> m (Maybe String)
loadPng file = liftIO $ do
  x <- readPng file
  case x of
    Left err -> return $ Just err
    Right i  -> unsafeImageData i >> return Nothing

loadTexturePure :: MonadIO m => FilePath -> m (Maybe GLuint)
loadTexturePure file = liftIO $ do
  handleptr <- new 0
  glGenTextures 1 handleptr
  handle <- peek handleptr
  glBindTexture GL_TEXTURE_2D handle
  x <- loadPng ("res" </> file)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
  glBindTexture GL_TEXTURE_2D 0
  case x of
    Just err -> putStrLn err >> return Nothing
    Nothing  -> return $ Just handle

loadTextureResource :: (MonadState s m, MonadIO m) =>
                    Lens' s Resources -> FilePath -> m ()
loadTextureResource res file = do
  tex <- loadTexturePure file
  res . textures . at file .= fmap toTexture tex

bindTexture2D :: MonadIO m => Maybe Texture -> m ()
bindTexture2D = glBindTexture GL_TEXTURE_2D . fromTexture . fromMaybe (Bind 0)
