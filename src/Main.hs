{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Text as T
import Linear hiding (ortho)
import qualified SDL

import Init.OpenGL
import Init.Resources
import Game
import Entity
import Resources
import Render.RenderInfo

windowConfig :: (T.Text, SDL.WindowConfig)
windowConfig = ("Space Sim", SDL.defaultWindow
               { SDL.windowInitialSize = V2 640 480
               , SDL.windowPosition    = SDL.Centered
               , SDL.windowOpenGL      = Just openGLConfig
               })

main :: IO ()
main = runGameT mainGame

mainGame :: GameT IO ()
mainGame = do
  (window, context) <- initWindow
  initResources
  mainLoop window
  destroyGame window context

initWindow :: MonadIO m => m (SDL.Window, SDL.GLContext)
initWindow = do
  SDL.initialize [SDL.InitVideo]
  window <- uncurry SDL.createWindow windowConfig
  context <- initGL window
  SDL.showWindow window
  return (window, context)
  
destroyGame :: MonadIO m => SDL.Window -> SDL.GLContext -> m ()
destroyGame window context = do
  SDL.glDeleteContext context
  SDL.destroyWindow window

mainLoop :: MonadIO m => SDL.Window -> GameT m ()
mainLoop window = do
  render
  SDL.glSwapWindow window
  event <- SDL.pollEvent
  case SDL.eventPayload <$> event of
    Just SDL.QuitEvent -> return ()
    _                  -> mainLoop window
  

render :: MonadIO m => GameT m ()
render = do
  applyCamera =<< use (renderInfo . camera)
  texs <- use (resources . textures)
  let tex      = texs ^. at "Rockfloor.png"
      entities = [tile (V2 x y) (V2 32 32) tex | x <- [0,32..640], y <- [0,32..640]]
  tex & bind
  glBegin GL_QUADS
  mapM_ renderEntity entities
  glEnd
  bind Nothing

renderEntity :: MonadIO m => Entity -> m ()
renderEntity entity = do
  glTexCoord2f 0 0
  glVertex2d (entity^.coord._x) (entity^.coord._y)
  glTexCoord2f 0 1
  glVertex2d (entity^.coord._x) (entity^.coord._y + entity^.size._y)
  glTexCoord2f 1 1
  glVertex2d (entity^.coord._x + entity^.size._x) (entity^.coord._y + entity^.size._y)
  glTexCoord2f 1 0
  glVertex2d (entity^.coord._x + entity^.size._x) (entity^.coord^._y)
