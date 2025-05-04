module Main where

import Prelude

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State qualified as State
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Time qualified as Time

import Block.Chain.State qualified as BlockCS
import Block.Chain.Types (Blockchain (..), BlockchainM)
import Block.Chain.Util qualified as BlockCU
import Transaction.Types (Transaction (..))

printBlockChainAndValue :: Blockchain -> IO ()
printBlockChainAndValue bc = do
  print "------"
  print $ "Blockchain: " <> show bc
  print "--"
  print $ "Value: " <> show (BlockCU.blockchainValue bc)
  print "------"

printBlockChainAndValueM :: BlockchainM IO ()
printBlockChainAndValueM = State.get >>= (liftIO . printBlockChainAndValue)

main :: IO ()
main = do
  currTime <- Time.getCurrentTime
  let initialTxns = Transaction 100 <| Transaction (-10) <| Transaction 5 :| []
      initBc = BlockCU.genesis currTime initialTxns

  printBlockChainAndValue initBc

  (_, finalBc) <- flip State.runStateT initBc $ do
    void . BlockCS.addBlockNowM $ Transaction 17 :| []
    printBlockChainAndValueM

    void . BlockCS.addBlockNowM $
      Transaction 1 <| Transaction 2 <| Transaction 3 :| []
    printBlockChainAndValueM

    void . BlockCS.addBlockNowM $ Transaction 54 <| Transaction (-54) :| []
    printBlockChainAndValueM

    void . BlockCS.addBlockNowM $ Transaction 12345 :| []
    printBlockChainAndValueM
    pure ()

  printBlockChainAndValue finalBc
