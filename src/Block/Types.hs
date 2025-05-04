module Block.Types where

import Prelude

import Crypto.Hash (Digest, SHA1)
import Data.List.NonEmpty (NonEmpty)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Transaction.Types (Transaction (..))

data Block = Block
  { prevBlockHash :: Maybe (Digest SHA1)
  , timestamp :: UTCTime
  , transactions :: NonEmpty Transaction
  }
  deriving stock (Eq, Show, Read, Generic)
