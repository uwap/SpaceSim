module Init.OpenGL where

import Graphics.Rendering.OpenGL

initGL :: IO ()
initGL = do
  texture Texture2D $= Enabled
  initProjection

initProjection :: IO ()
initProjection = do
  matrixMode $= Projection
  loadIdentity
  ortho 0 640 480 0 (-1) 1
  matrixMode $= Modelview 0
  loadIdentity
