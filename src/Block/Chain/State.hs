module Block.Chain.State where

import Prelude

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State qualified as State
import Data.List.NonEmpty (NonEmpty)
import Data.Time (UTCTime)
import Data.Time qualified as Time

import Block.Chain.Types (BlockchainM)
import Block.Chain.Util (addBlock)
import Transaction.Types (Transaction (..))

addBlockM ::
  (MonadIO m) =>
  UTCTime ->
  NonEmpty Transaction ->
  BlockchainM m ()
addBlockM time txns = do
  bc <- State.get
  State.put $ addBlock bc time txns

addBlockNowM ::
  (MonadIO m) =>
  NonEmpty Transaction ->
  BlockchainM m UTCTime
addBlockNowM txns = do
  now <- liftIO Time.getCurrentTime
  addBlockM now txns
  pure now
