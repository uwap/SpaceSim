{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Entity where

import Game
import Resources
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Linear

data Entity = Entity { _coord   :: !(V2 Double)
                     , _size    :: !(V2 Double)
                     , _zlevel  :: !Int
                     , _texture :: !(Maybe Texture)
                     , _update  :: !(Entity -> Game Entity)
                     }
makeLenses ''Entity

tile :: V2 Double -> Texture -> Entity
tile coord tex = Entity coord (V2 64 64) 0 (Just tex) pure

--        :: Lens' GameInfo [Entity] -> Lens' Entity (Entity -> Game Entity) -> Game ()
updateAll :: (MonadState s m, Traversable t) => Lens' s (t e) -> Lens' e (e -> m e) -> m ()
updateAll entities update = do
  all <- use entities
  entities <~ traverse (\x -> x & x ^. update) all
