{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
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

data Resources = Resources
               { _textures :: Map String GLuint }
makeLenses ''Resources

mkResources :: Resources
mkResources = Resources empty

unsafeImageData :: DynamicImage -> IO ()
unsafeImageData (ImageRGB8 i)   = V.unsafeWith (imageData i) (glTexImage2D GL_TEXTURE_2D 0 GL_RGB  (fromIntegral $ imageWidth i) (fromIntegral $ imageHeight i) 0 GL_RGB  GL_UNSIGNED_BYTE  . castPtr)
unsafeImageData (ImageRGB16 i)  = V.unsafeWith (imageData i) (glTexImage2D GL_TEXTURE_2D 0 GL_RGB  (fromIntegral $ imageWidth i) (fromIntegral $ imageHeight i) 0 GL_RGB  GL_UNSIGNED_SHORT . castPtr)
unsafeImageData (ImageRGBA8 i)  = V.unsafeWith (imageData i) (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA (fromIntegral $ imageWidth i) (fromIntegral $ imageHeight i) 0 GL_RGBA GL_UNSIGNED_BYTE  . castPtr)
unsafeImageData (ImageRGBA16 i) = V.unsafeWith (imageData i) (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA (fromIntegral $ imageWidth i) (fromIntegral $ imageHeight i) 0 GL_RGBA GL_UNSIGNED_SHORT . castPtr)
unsafeImageData _               = error "uwap was too lazy"

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
  res . textures . at file .= tex

bindTexture2D :: MonadIO m => Maybe GLuint -> m ()
bindTexture2D = glBindTexture GL_TEXTURE_2D . fromMaybe 0
