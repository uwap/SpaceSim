{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Resources where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Map
import Data.Maybe
import System.FilePath
import Init.OpenGL

data Resources = Resources
               { _textures :: Map String GLuint }
makeLenses ''Resources

mkResources :: Resources
mkResources = Resources empty

loadTexturePure :: MonadIO m => FilePath -> m (Maybe GLuint)
loadTexturePure file = return $ return 0
  {-liftIO $ do
  tex <- readTexture ("res" </> file)
  textureFilter Texture2D $= ((Linear', Nothing), Linear')
  texture2DWrap $= (Mirrored, ClampToEdge)
  return $ either (const Nothing) Just tex
  -}
loadTextureResource :: (MonadState s m, MonadIO m) =>
                    Lens' s Resources -> FilePath -> m ()
loadTextureResource res file = do
  tex <- loadTexturePure file
  res . textures . at file .= tex

bindTexture2D :: MonadIO m => Maybe GLuint -> m ()
bindTexture2D = glBindTexture GL_TEXTURE_2D . fromMaybe 0
