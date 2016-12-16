{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Control.Monad.Identity 

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.Functor
import Data.List
import qualified Data.Map as M
import Data.Monoid
import qualified Database.LevelDB as LD
import Blockchain.VM.VMState
import Blockchain.Output
import Blockchain.VMContext
import Control.Monad.Logger

import Control.Monad.Trans.Class
import Numeric

import System.Directory
import System.Exit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Aeson
import Data.Maybe
import Data.Either
import Data.List
import HFlags

import TestEthereum
import TestFiles
import qualified TestDescriptions as TD

doTest :: [(String, TD.Test)] -> IO ()
doTest tests = do
  flip runLoggingT noLog $ runContextM $ forM tests $ \(name, test) -> do 
    result <- runTest test
    _ <- liftIO $ runTestTT $ TestList $ fmap (\(n, r) -> (TestLabel n (TestCase $ assertBool "id" (isRight r)))) [(name, result)]
    return (name, result)

-- doTests :: [(String, TD.Test)] -> IO () 
-- doTests tests = do
--   flip runLoggingT noLog $ runContextM $ runTestTT $ TestList $ fmap (\(n, t) -> (TestLabel n (TestCase $ assertBool "id" (lift $ isRight (runTest t))))) tests 

main::IO ()
main = do
  args <- $initHFlags "The Ethereum Test program"
  testsExist <- doesDirectoryExist "tests"
  when (not testsExist) $
    error "You need to clone the git repository at https://github.com/ethereum/tests.git"

  res <- forM testFiles $ \theFileName -> do
    theFile <- BL.readFile theFileName
    --putStrLn $ "#### Running tests in file: " ++ theFileName
    --counts <- runTestTT $ TestList [ TestLabel theFileName (TestCase $ assertBool "id" True)]
    case fmap fromJSON $ eitherDecode theFile::Either String (Result TD.Tests) of
          Left err ->  putStrLn ("error: " ++ err)
          Right val ->
            case val of
              Error err' -> putStrLn ("error': " ++ err')
              Success tests -> doTest $ (M.toList tests) 
    return ()

  return ()

