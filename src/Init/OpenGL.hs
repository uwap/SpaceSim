module Init.OpenGL (
  initGL,
  module Graphics.GL.Standard30,
  module Graphics.GL.Types
) where

import Control.Monad.IO.Class
import Graphics.GL.Standard30
import Graphics.GL.Types

initGL :: MonadIO m => m ()
initGL = do
  glEnable GL_TEXTURE_2D
  initProjection

initProjection :: MonadIO m => m ()
initProjection = do
  glMatrixMode GL_PROJECTION
  glLoadIdentity
  glOrtho 0 640 480 0 (-1) 1
  glMatrixMode GL_MODELVIEW
  glLoadIdentity
