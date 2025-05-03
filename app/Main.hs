module Main where

import Prelude

import Data.List.NonEmpty qualified as NE
import Data.Time qualified as Time

import Block.Chain.Types (Blockchain (..), addBlock, blockchainValue, genesis)
import Transaction.Types (Transaction (..))

printBlockChainAndValue :: Blockchain -> IO ()
printBlockChainAndValue bc = do
  print "------"
  print $ "Blockchain: " <> show bc
  print "--"
  print $ "Value: " <> show (blockchainValue bc)
  print "------"

main :: IO ()
main = do
  currTime <- Time.getCurrentTime
  let initialTxns =
        NE.fromList
          [Transaction 100, Transaction (-10), Transaction 5]
      bc1 = genesis currTime initialTxns
  bc2 <-
    fmap
      (\time -> addBlock bc1 time (NE.fromList [Transaction 17]))
      Time.getCurrentTime
  bc3 <-
    fmap
      ( \time ->
          addBlock bc2 time (NE.fromList [Transaction 1, Transaction 2, Transaction 3])
      )
      Time.getCurrentTime
  bc4 <-
    fmap
      (\time -> addBlock bc3 time (NE.fromList [Transaction 54, Transaction (-54)]))
      Time.getCurrentTime
  bc5 <-
    fmap
      (\time -> addBlock bc4 time (NE.fromList [Transaction 12345]))
      Time.getCurrentTime
  printBlockChainAndValue bc1
  printBlockChainAndValue bc2
  printBlockChainAndValue bc3
  printBlockChainAndValue bc4
  printBlockChainAndValue bc5
