{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import Control.Monad.State.Strict
import Graphics.Rendering.OpenGL
import qualified Data.Text as T
import Linear hiding (ortho)
import qualified SDL

import Init.OpenGL
import Game
import Resources

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

  initGL

  SDL.showWindow window
  runGameT $ do
    loadTextureResource resources "Rockfloor.png"
    mainLoop window
  SDL.glDeleteContext context
  SDL.destroyWindow window

mainLoop :: SDL.Window -> GameT IO ()
mainLoop window = do
  render
  SDL.glSwapWindow window
  event <- SDL.pollEvent
  case SDL.eventPayload <$> event of
    Just SDL.QuitEvent -> return ()
    _                  -> mainLoop window
  

render :: GameT IO ()
render = do
  let coords = [V2 x y | x <- [0,64..640], y <- [0,64..640]]
  texs <- use (resources . textures)
  liftIO $ do
    textureBinding Texture2D $= texs ^. at "Rockfloor.png"
    renderPrimitive Quads $ mapM_ renderTile coords
    textureBinding Texture2D $= Nothing

renderTile :: V2 GLfloat -> IO ()
renderTile coord = do
  texCoord $ TexCoord2 0 (0 :: GLint)
  vertex   $ Vertex2 (coord^._x) (coord^._y)
  texCoord $ TexCoord2 0 (1 :: GLint)
  vertex   $ Vertex2 (coord^._x) (coord^._y + 64)
  texCoord $ TexCoord2 1 (1 :: GLint)
  vertex   $ Vertex2 (coord^._x + 64) (coord^._y + 64)
  texCoord $ TexCoord2 1 (0 :: GLint)
  vertex   $ Vertex2 (coord^._x + 64) (coord^._y)
