{-# LANGUAGE TemplateHaskell #-}
module Game where

import Resources
import Render.RenderInfo
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Identity
import System.Random

data GameInfo = GameInfo
              { _randomGen  :: !StdGen
              , _resources  :: !Resources
              , _renderInfo :: !RenderInfo
              }
makeLenses ''GameInfo

mkGameInfo :: MonadIO m => m GameInfo
mkGameInfo = mkGameInfo' <$> liftIO newStdGen

mkGameInfo' :: StdGen -> GameInfo
mkGameInfo' ran = GameInfo ran mkResources mkRenderInfo

type GameT m a = StateT GameInfo m a
type Game a    = GameT Identity a

runGame :: Int -> Game a -> a
runGame seed f = runIdentity $ evalStateT f (mkGameInfo' $ mkStdGen seed)

runGameT :: MonadIO m => GameT m a -> m a
runGameT f = evalStateT f =<< mkGameInfo

lift :: Applicative m => Game a -> GameT m a
lift = mapStateT (pure . runIdentity)
