{-# LANGUAGE TemplateHaskell #-}
module Render.Camera where

import Control.Lens
import Control.Monad.IO.Class
import Init.OpenGL
import Linear

type Scale          = V2 Double
type Rotation       = V2 Double
type Transformation = V2 Double

data Camera = Camera { _scale          :: Scale
                     , _rotation       :: Rotation
                     , _transformation :: Transformation
                     }
makeLenses ''Camera

mkCamera :: Camera
mkCamera = Camera (V2 1 1) (V2 0 0) (V2 0 0)

applyCamera :: MonadIO m => Camera -> m ()
applyCamera cam = return ()
