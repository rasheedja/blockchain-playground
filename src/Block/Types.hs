module Block.Types where

import Prelude

import Crypto.Hash (Digest, SHA1)
import Crypto.Hash qualified as Hash
import Data.ByteString.Char8 qualified as BS
import Data.List.NonEmpty (NonEmpty)
import Data.Time qualified as Time
import GHC.Generics (Generic)

import Transaction.Types (Transaction (..))

data Block = Block
  { prevBlockHash :: Maybe (Hash.Digest Hash.SHA1)
  , timestamp :: Time.UTCTime
  , transactions :: NonEmpty Transaction
  }
  deriving stock (Eq, Show, Read, Generic)

blockValue :: Block -> Integer
blockValue block =
  sum $ fmap (.amount) block.transactions

blockHash :: Block -> Digest SHA1
blockHash b =
  Hash.hashWith Hash.SHA1 . BS.pack $ show b.timestamp <> show b.transactions
