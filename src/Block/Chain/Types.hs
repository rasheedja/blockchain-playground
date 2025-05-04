module Block.Chain.Types where

import Prelude

import Control.Monad.State (StateT)
import GHC.Generics

import Block.Types (Block (..))

data Blockchain = Blockchain
  { block :: Block
  , chain :: [Blockchain]
  }
  deriving stock (Eq, Show, Read, Generic)

type BlockchainM m = StateT Blockchain m
