{-# LANGUAGE DeriveGeneric, OverloadedStrings, FlexibleInstances, TemplateHaskell #-}

import HFlags
import Control.Monad
import Control.Monad.Logger
import System.Directory

import System.Environment
import Blockchain.Output
--import Blockchain.VM.VMState
import Blockchain.VMContext
import Blockchain.VMOptions

import TestEthereum

main:: IO ()
main = do
  _ <- $initHFlags "VMTests"
  testsExist <- doesDirectoryExist "tests"
  when (not testsExist) $
    error "You need to clone the git repository at https://github.com/ethereum/tests.git"

  args <- getArgs

  let (maybeFileName, maybeTestName) = 
        case args of
          [] -> (Nothing, Nothing)
          [x] -> (Just x, Nothing)
          [x, y] -> (Just x, Just y)
          _ -> error "You can only supply 2 parameters"
  
  _ <- flip runLoggingT printLogMsg $ runContextM $ do
    runAllTests maybeFileName maybeTestName
  
  return ()


