{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Resources where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Graphics.GLUtil
import Graphics.Rendering.OpenGL
import Data.Map
import System.FilePath

data Resources = Resources
               { _textures :: Map String TextureObject }
makeLenses ''Resources

mkResources :: Resources
mkResources = Resources empty

loadTexturePure :: MonadIO m => FilePath -> m (Maybe TextureObject)
loadTexturePure file = liftIO $ do
  tex <- readTexture ("res" </> file)
  textureFilter Texture2D $= ((Linear', Nothing), Linear')
  texture2DWrap $= (Mirrored, ClampToEdge)
  return $ either (const Nothing) Just tex
  
loadTextureResource :: (MonadState s m, MonadIO m) =>
                    Lens' s Resources -> FilePath -> m ()
loadTextureResource res file = do
  tex <- loadTexturePure file
  res . textures . at file .= tex
