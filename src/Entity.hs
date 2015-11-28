{-# LANGUAGE TemplateHaskell #-}
module Entity where

import Game
import Graphics.Rendering.OpenGL (TextureObject)
import Control.Lens
import Linear

data Entity = Entity { _coord   :: !(V2 Double)
                     , _size    :: !(V2 Double)
                     , _zlevel  :: !Int
                     , _texture :: !(Maybe TextureObject)
                     , _update  :: !(Entity -> Game Entity)
                     }
makeLenses ''Entity

tile :: V2 Double -> TextureObject -> Entity
tile coord tex = Entity coord (V2 64 64) 0 (Just tex) pure
