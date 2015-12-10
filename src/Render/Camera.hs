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

apply :: MonadIO m => Camera -> m ()
apply cam = return ()
