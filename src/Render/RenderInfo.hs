{-# LANGUAGE TemplateHaskell #-}
module Render.RenderInfo
  ( RenderInfo (..)
  , mkRenderInfo
  , camera
  , module Render.Camera
  ) where

import Render.Camera
import Control.Lens

data RenderInfo = RenderInfo
                { _camera :: Camera }
makeLenses ''RenderInfo

mkRenderInfo :: RenderInfo
mkRenderInfo = RenderInfo mkCamera
