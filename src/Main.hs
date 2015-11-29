{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Text as T
import Linear hiding (ortho)
import qualified SDL

import Init.OpenGL
import Game
import Resources

windowConfig :: (T.Text, SDL.WindowConfig)
windowConfig = ("Space Sim", SDL.defaultWindow
               { SDL.windowInitialSize = V2 640 480
               , SDL.windowPosition    = SDL.Centered
               , SDL.windowOpenGL      = Just openGLConfig
               })

main :: IO ()
main = runGameT initGame

initGame :: GameT IO ()
initGame = do
  SDL.initialize [SDL.InitVideo]
  window <- uncurry SDL.createWindow windowConfig
  context <- initGL window
  SDL.showWindow window
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
  bindTexture2D (texs ^. at "Rockfloor.png")
  glBegin GL_QUADS
  mapM_ renderTile coords
  glEnd
  bindTexture2D Nothing

renderTile :: MonadIO m => V2 Float -> m ()
renderTile coord = do
  glTexCoord2f 0 0
  glVertex2f (coord^._x) (coord^._y)
  glTexCoord2f 0 1
  glVertex2f (coord^._x) (coord^._y + 64)
  glTexCoord2f 1 1
  glVertex2f (coord^._x + 64) (coord^._y + 64)
  glTexCoord2f 1 0
  glVertex2f (coord^._x + 64) (coord^._y)
