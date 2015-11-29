module Init.Resources where

import Control.Monad.IO.Class
import Resources
import Game

initResources :: MonadIO m => GameT m ()
initResources = loadTextureResource resources "Rockfloor.png"
