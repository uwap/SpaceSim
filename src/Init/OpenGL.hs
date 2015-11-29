module Init.OpenGL (
  initGL, openGLConfig,
  module Graphics.GL.Standard30,
  module Graphics.GL.Types
) where

import Control.Monad.IO.Class
import Graphics.GL.Standard30
import Graphics.GL.Types
import qualified SDL

openGLConfig :: SDL.OpenGLConfig
openGLConfig = SDL.defaultOpenGL
             { SDL.glProfile = SDL.Compatibility SDL.Debug 3 0 }
             
initGL :: MonadIO m => SDL.Window -> m SDL.GLContext
initGL window = do
  context <- SDL.glCreateContext window
  SDL.glMakeCurrent window context
  glEnable GL_TEXTURE_2D
  initProjection
  return context

initProjection :: MonadIO m => m ()
initProjection = do
  glMatrixMode GL_PROJECTION
  glLoadIdentity
  glOrtho 0 640 480 0 (-1) 1
  glMatrixMode GL_MODELVIEW
  glLoadIdentity
