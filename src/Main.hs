{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Rendering.OpenGL
import qualified Data.Text as T
import Linear hiding (ortho)
import qualified SDL

openGLConfig :: SDL.OpenGLConfig
openGLConfig = SDL.defaultOpenGL
             { SDL.glProfile = SDL.Compatibility SDL.Debug 3 2 }

windowConfig :: (T.Text, SDL.WindowConfig)
windowConfig = ("Space Sim", SDL.defaultWindow
               { SDL.windowInitialSize = V2 640 480
               , SDL.windowPosition    = SDL.Centered
               , SDL.windowOpenGL      = Just openGLConfig
               })

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- uncurry SDL.createWindow windowConfig
  context <- SDL.glCreateContext window
  SDL.glMakeCurrent window context

  texture Texture2D $= Enabled

  matrixMode $= Projection
  loadIdentity
  ortho 0 640 480 0 (-1) 1
  matrixMode $= Modelview 0
  loadIdentity

  SDL.showWindow window
  mainLoop window
  SDL.glDeleteContext context
  SDL.destroyWindow window

mainLoop :: SDL.Window -> IO ()
mainLoop window = do
  render
  SDL.glSwapWindow window
  event <- SDL.pollEvent
  case SDL.eventPayload <$> event of
    Just SDL.QuitEvent -> return ()
    _                  -> mainLoop window
  

render :: IO ()
render =
  renderPrimitive Quads $ do
    vertex $ Vertex2 0 (0 :: GLfloat)
    vertex $ Vertex2 0 (480 :: GLfloat)
    vertex $ Vertex2 640 (480 :: GLfloat)
    vertex $ Vertex2 640 (0 :: GLfloat)
