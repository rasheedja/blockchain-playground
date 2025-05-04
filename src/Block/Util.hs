module Block.Util where

import Prelude

import Crypto.Hash (Digest, SHA1)
import Crypto.Hash qualified as Hash
import Data.ByteString.Char8 qualified as BS

import Block.Types (Block (..))
import Transaction.Types (Transaction (..))

blockValue :: Block -> Integer
blockValue block =
  sum $ fmap (.amount) block.transactions

blockHash :: Block -> Digest SHA1
blockHash b =
  Hash.hashWith Hash.SHA1 . BS.pack $ show b.timestamp <> show b.transactions
