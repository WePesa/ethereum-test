{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad
import Control.Monad.Identity 

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.Default
import Data.Functor
import Data.List
import qualified Data.Map as M
import Data.Monoid
import qualified Database.LevelDB as LD

import Numeric

import System.Exit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

main::IO ()
main = do
--   counts <- runTestTT $ TestList [TestLabel " get . put = id" testGetPut,
--                                   TestLabel " get . put . put = id" testGetPutRepeated,
--                                   TestLabel " get . putn = id" testGetPutRepeatedII,
--                                   TestLabel " single insert" testSingleInsert,
--                                   TestLabel " multiple insert" testMultipleInserts]
-- 
  return ()

