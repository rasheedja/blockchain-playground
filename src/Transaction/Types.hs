module Transaction.Types where

import Prelude

import GHC.Generics (Generic)

newtype Transaction = Transaction {amount :: Integer}
  deriving stock (Eq, Show, Read, Generic)
