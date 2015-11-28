{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.FilePath
import Control.Lens
import Graphics.GLUtil
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
  mainLoop window =<< loadTextureFile "Rockfloor.png"
  SDL.glDeleteContext context
  SDL.destroyWindow window

mainLoop :: SDL.Window -> Maybe TextureObject -> IO ()
mainLoop window tex = do
  render tex
  SDL.glSwapWindow window
  event <- SDL.pollEvent
  case SDL.eventPayload <$> event of
    Just SDL.QuitEvent -> return ()
    _                  -> mainLoop window tex
  

render :: Maybe TextureObject -> IO ()
render tex = do
  let coords = [V2 x y | x <- [0,64..640], y <- [0,64..640]]
  textureBinding Texture2D $= tex
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

loadTextureFile :: FilePath -> IO (Maybe TextureObject)
loadTextureFile file = do
  tex <- readTexture ("res" </> file)
  textureFilter Texture2D $= ((Linear', Nothing), Linear')
  texture2DWrap $= (Mirrored, ClampToEdge)
  return $ either (const Nothing) Just tex
