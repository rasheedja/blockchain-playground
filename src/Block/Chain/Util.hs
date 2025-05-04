module Block.Chain.Util where

import Prelude

import Data.List.NonEmpty (NonEmpty)
import Data.Time (UTCTime)

import Block.Chain.Types (Blockchain (..))
import Block.Types (Block (..))
import Block.Util qualified as BlockU
import Transaction.Types (Transaction (..))

consensus :: [Blockchain] -> Maybe Blockchain
consensus [] = Nothing
consensus (b : _) = Just b

canonicalBlocks :: Blockchain -> [Block]
canonicalBlocks bc =
  bc.block : maybe [] canonicalBlocks (consensus bc.chain)

blockchainValue :: Blockchain -> Integer
blockchainValue bc =
  foldr ((+) . BlockU.blockValue) 0 (canonicalBlocks bc)

addBlock :: Blockchain -> UTCTime -> NonEmpty Transaction -> Blockchain
addBlock prev time txns =
  let currBlockHash = BlockU.blockHash prev.block
      newBlock =
        Block
          { prevBlockHash = Just currBlockHash
          , timestamp = time
          , transactions = txns
          }
  in  Blockchain {block = newBlock, chain = prev : prev.chain}

genesis :: UTCTime -> NonEmpty Transaction -> Blockchain
genesis time txns =
  Blockchain
    { block =
        Block
          { prevBlockHash = Nothing
          , timestamp = time
          , transactions = txns
          }
    , chain = []
    }
