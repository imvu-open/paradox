module Paradox.State where

import Control.Concurrent.MVar
import Control.Monad.IO.Class

import Control.Monad.Trans.Class                (MonadTrans)

import qualified Paradox.Types                  as PT


withState :: MonadTrans t => (PT.MutableState -> PT.InnerMonadM a) -> t PT.InnerMonadM a
withState = PT.doWithState

getMutableState :: (MonadTrans t) => t PT.InnerMonadM PT.ServerState
getMutableState = PT.getFromState (liftIO . readMVar)

getMutableState' :: (MonadTrans t) => (PT.ServerState -> c) -> t PT.InnerMonadM c
getMutableState' extract = PT.getFromState (fmap extract . liftIO . readMVar)
