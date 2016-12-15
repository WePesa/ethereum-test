{-# LANGUAGE DeriveGeneric, OverloadedStrings, FlexibleInstances, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module TestEthereum (
                      runTest
                    , runAllTests
                    ) where

--import Control.Applicative
import Control.Monad
import Control.Monad.Logger
--import Control.Monad.IfElse
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Either
--import Control.Monad.Trans.Resource
import Control.Monad.Trans.State
import qualified Crypto.Hash.SHA3 as SHA3
import Data.Aeson
import qualified Data.Binary as Bin
import qualified Data.ByteString as B
--import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Either
import qualified Data.Set as Set
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Network.Haskoin.Internals as Haskoin
import Network.Haskoin.Crypto (withSource)
import Numeric
import System.Environment
import System.Directory
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Blockchain.Output

import Blockchain.BlockChain
import qualified Blockchain.Colors as C
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.Data.BlockDB
import Blockchain.Data.Code
import Blockchain.VMContext
--import Blockchain.Data.DataDefs
import Blockchain.Data.RLP
import Blockchain.Data.Transaction
import Blockchain.Data.TransactionDef
import Blockchain.Database.MerklePatricia
import Blockchain.DB.CodeDB
import Blockchain.DB.StateDB
--import Blockchain.DBM
import Blockchain.ExtWord
import Blockchain.Format
import Blockchain.SHA
import Blockchain.Util
import Blockchain.VM
import Blockchain.VM.Code
import Blockchain.VM.Environment
import Blockchain.VM.VMState
import Blockchain.VMOptions
import qualified Data.NibbleString as N
import Blockchain.DB.MemAddressStateDB
import Blockchain.DB.StorageDB
import Blockchain.Database.MerklePatricia.Internal
import Blockchain.FastECRecover
--import Blockchain.Data.Address
import Blockchain.ExtendedECDSA
--import Data.Maybe (fromJust)
import HFlags 

import TestDescriptions

--import Debug.Trace

import TestFiles

--nibbleString2ByteString::N.NibbleString->B.ByteString
--nibbleString2ByteString (N.EvenNibbleString str) = str
--nibbleString2ByteString (N.OddNibbleString c str) = c `B.cons` str

emptyStringChar8BS :: Int -> BC.ByteString
emptyStringChar8BS len = BC.pack $ replicate len '\0'

isDebugEnabled :: Bool
isDebugEnabled = True

populateAndConvertAddressState::Address->AddressState'->ContextM AddressState
populateAndConvertAddressState owner addressState' = do
  addCode . codeBytes . contractCode' $ addressState' -- lift

  forM_ (M.toList $ storage' addressState') $
    \(key, val) -> do putStorageKeyVal' owner (fromIntegral key) (fromIntegral val)

  addressState <- getAddressState owner -- lift

  return $
    AddressState
      (nonce' addressState')
      (balance' addressState')
      (addressStateContractRoot addressState)
      (hash $ codeBytes $ contractCode' addressState')





showHexInt::Integer->String
showHexInt x
  = let xHex = showHex x ""
    in (if odd $ length xHex
        then "0x0"
        else "0x")
       ++ xHex

getDataAndRevertAddressState::Address->AddressState->ContextM AddressState'
getDataAndRevertAddressState _ addressState = do
  theCode <- fmap (fromMaybe (error $ "Missing code in getDataAndRevertAddressState: " ++ format addressState)) $
             getCode (addressStateCodeHash addressState) -- lift

  -- Copied wholesale from Context.hs:getAllStorageKeyVals'
  -- since that function requires an unhashed owner.
  -- This piece of code really should be in the lib somewhere
  storage <- do
    dbs <- get -- lift
    let mpdb = (contextStateDB dbs){stateRoot=addressStateContractRoot addressState}
    kvs <- unsafeGetKeyVals mpdb "" -- lift $ lift 
    return $ map (fmap $ fromInteger . rlpDecode . rlpDeserialize . rlpDecode) kvs
    
  return $
    AddressState'
    (addressStateNonce addressState)
    (addressStateBalance addressState)
    (M.mapKeys (byteString2Integer . nibbleString2ByteString)
     . M.map (fromIntegral)
     $ M.fromList storage)
    (Code theCode)

--formatAddressState::AddressState'->String
--formatAddressState = show

getNumber::String->Integer
getNumber "" = 0
getNumber x = read x

--newAccountsToCallCreates::(Maybe Address, Integer, AddressState)->ContextM DebugCallCreate
--newAccountsToCallCreates (maybeAddress, gasRemaining, AddressState{balance=b, codeHash=h}) = do
--  Just codeBytes <- lift $ getCode h
--  let destination =
--        case maybeAddress of
--          Just (Address address) -> padZeros 40 $ showHex address ""
--          Nothing -> ""
--  return DebugCallCreate {
--    ccData="0x" ++ BC.unpack (B16.encode codeBytes),
--    ccDestination=destination,
--    ccGasLimit=show gasRemaining,
--    ccValue=show b
--    }

--isBlankCode::Code->Bool
--isBlankCode (Code "") = True
--isBlankCode _ = False

showInfo::(Address,AddressState')->String
showInfo (key,AddressState'{nonce'=n, balance'=b, storage'=s, contractCode'=Code c}) = 
    show (pretty key) ++ "[#ed]" ++ "(" ++ show n ++ "): " ++ show b ++ 
         (if M.null s
          then ""
          else ", " ++ (show $ M.toList $
               M.map showHexInt $ M.mapKeys ((++ "[#ed]") . showHexInt) s)
         ) ++ 
         (if B.null c then "" else ", CODE:[" ++ C.blue (format c) ++ "]")
showInfo _ = ""  

addressStates::ContextM [(Address, AddressState')]
addressStates = do
  addrStates <- getAllAddressStates -- lift
  let addrs = map fst addrStates
      states = map snd addrStates
  states' <- mapM (uncurry getDataAndRevertAddressState) $ zip addrs states
  return $ zip addrs states'

--runTest'::Test->ContextM (Either String String)
--runTest' test = do
--    return $ Left ""

runTest::Test->ContextM (Either String String)
runTest test = do

  initializeBlank =<< getStateDB 
  setStateDBStateRoot emptyTriePtr
  --lift $ setStateRoot emptyTriePtr -- TODO add me again

  forM_ (M.toList $ pre test) $
    \(addr, s) -> do
      state <- populateAndConvertAddressState addr s
      putAddressState addr state -- lift

  beforeAddressStates <- addressStates

  let block =
        Block {
          blockBlockData = BlockData {
             blockDataParentHash = previousHash . env $ test,
             blockDataNumber = read . currentNumber . env $ test,
             blockDataCoinbase = currentCoinbase . env $ test,
             blockDataDifficulty = read . currentDifficulty . env $ test,
             blockDataUnclesHash = SHA $ fromIntegral 0, -- error "unclesHash undefined",
             blockDataStateRoot = StateRoot $ emptyStringChar8BS 32, -- error "bStateRoot undefined",
             blockDataTransactionsRoot = StateRoot $ emptyStringChar8BS 32, --error "transactionsRoot undefined",
             blockDataReceiptsRoot = StateRoot $ emptyStringChar8BS 32, --error "receiptsRoot undefined",
             blockDataLogBloom = emptyStringChar8BS 32, -- error "logBloom undefined",
             blockDataGasLimit = currentGasLimit . env $ test,
             blockDataGasUsed = 0, -- error "gasUsed undefined",
             blockDataTimestamp = currentTimestamp . env $ test,
             --timestamp = posixSecondsToUTCTime . fromInteger . read . currentTimestamp . env $ test,
             blockDataExtraData = 0, -- error "extraData undefined",
             blockDataNonce = 0, -- error "nonce undefined",
             blockDataMixHash = SHA $ fromIntegral 0 -- error "mixHash undefined" 
             },
          blockReceiptTransactions = [], -- error "receiptTransactions undefined",
          blockBlockUncles = [] -- error "blockUncles undefined"
          }

  (result, retVal, gasRemaining, logs, returnedCallCreates) <-
    case theInput test of
      IExec exec -> do

        let env =
              Environment{
                envGasPrice=getNumber $ gasPrice' exec,
                envBlock=block,
                envOwner = address' exec,
                envOrigin = origin exec,
                envInputData = theData $ data' exec,
                envSender = caller exec,
                envValue = getNumber $ value' exec,
                envCode = code exec,
                envJumpDests = getValidJUMPDESTs $ code exec
                }
        ctx <- get
        vmState <- liftIO $ startingState False False env ctx -- (error "Context not defined") -- TODO add Context here

        (result, vmState) <-
          lift $ flip runStateT vmState{vmGasRemaining=getNumber $ gas exec, debugCallCreates=Just []} $ -- - lift
          runEitherT $ do
            runCodeFromStart

            vmState <- lift get 
            when isDebugEnabled $ do -- liftM
              liftIO $ putStrLn $ "Removing accounts in suicideList: " ++ 
                                 intercalate ", " (show . pretty <$> (Set.toList $ suicideList vmState))

            forM_ (suicideList vmState) $ deleteAddressState -- lift . lift . lift

        return (result, returnVal vmState, vmGasRemaining vmState, logs vmState, debugCallCreates vmState)

      ITransaction transaction -> do
        let t = case tTo' transaction of
                Nothing ->
                  createContractCreationTX
                    (getNumber $ tNonce' transaction)
                    (getNumber $ tGasPrice' transaction)
                    (getNumber $ tGasLimit' transaction)
                    (getNumber $ tValue' transaction)
                    (Code $ theData $ tData' transaction)
                    (tSecretKey' transaction)
                Just a ->
                  createMessageTX
                    (getNumber $ tNonce' transaction)
                    (getNumber $ tGasPrice' transaction)
                    (getNumber $ tGasLimit' transaction)
                    a
                    (getNumber $ tValue' transaction)
                    (theData $ tData' transaction)
                    (tSecretKey' transaction)
        signedTransaction <- liftIO $ withSource Haskoin.devURandom t 
        result <-
          runEitherT $ addTransaction False block (currentGasLimit $ env test) signedTransaction

        case result of
          Right (vmState, _) ->
            return (Right (), returnVal vmState, vmGasRemaining vmState, logs vmState, debugCallCreates vmState)
          Left e -> return (Right (), Nothing, 0, [], Just [])


  afterAddressStates <- addressStates

  let hashInteger = byteString2Integer . nibbleString2ByteString . N.EvenNibbleString . (SHA3.hash 256) . nibbleString2ByteString . N.pack . (N.byte2Nibbles =<<) . word256ToBytes . fromIntegral
      hashAddress (Address s) = Address $ fromIntegral $ byteString2Integer $ nibbleString2ByteString $ N.EvenNibbleString $ (SHA3.hash 256) $ BL.toStrict $ Bin.encode s

  let postTest = M.toList $
                 M.mapKeys hashAddress $
                 flip M.map (post test) $
                 \s' -> s'{storage' = M.mapKeys hashInteger (storage' s')} 

  when isDebugEnabled $ do -- whenM
    liftIO $ putStrLn "\nBefore   -------------"
    liftIO $ putStrLn $ unlines $ showInfo <$> beforeAddressStates
    liftIO $ putStrLn "After    -------------"
    liftIO $ putStrLn $ unlines $ showInfo <$> afterAddressStates
    liftIO $ putStrLn "Expected -------------"
    liftIO $ putStrLn $ unlines $ showInfo <$> postTest
    liftIO $ putStrLn "End      -------------\n"

  case (RawData (fromMaybe B.empty retVal) == out test,
        (afterAddressStates == postTest) || (null postTest && isLeft result),
        case remainingGas test of
          Nothing -> True
          Just x -> gasRemaining == x,
        logs == reverse (logs' test),
        (callcreates test == fmap reverse returnedCallCreates) || (isNothing (callcreates test) && (returnedCallCreates == Just []))
        ) of
    (False, _, _, _, _) -> return $ Left "result doesn't match"
    (_, False, _, _, _) -> return $ Left "address states don't match"
    (_, _, False, _, _) -> return $ Left $ "remaining gas doesn't match: is " ++ show gasRemaining ++ ", should be " ++ show (remainingGas test) ++ ", diff=" ++ show (gasRemaining - fromJust (remainingGas test))
    (_, _, _, False, _) -> do
      liftIO $ putStrLn "llllllllllllllllllllll"
      liftIO $ putStrLn $ show $ logs
      liftIO $ putStrLn "llllllllllllllllllllll"
      liftIO $ putStrLn $ show $ logs' test
      liftIO $ putStrLn "llllllllllllllllllllll"
      return $ Left "logs don't match"
    (_, _, _, _, False) -> do
      liftIO $ do
        putStrLn $ "callcreates test = " ++ show (callcreates test)
        putStrLn $ "returnedCallCreates = " ++ show returnedCallCreates
      
      return $ Left $ "callcreates don't match"
    _ -> return $ Right "Success"

formatResult::(String, Either String String)->String
formatResult (name, Left err) = "> " ++ name ++ ": " ++ C.red err
formatResult (name, Right message) = "> " ++ name ++ ": " ++ C.green message

runTests::[(String, Test)]->ContextM ()
runTests tests = do
  results <- 
    forM tests $ \(name, test) -> do
      liftIO $ putStrLn $ "Running test: " ++ show name
      result <- runTest test
      return (name, result)
  liftIO $ putStrLn $ intercalate "\n" $ formatResult <$> results

whoSignedThisTransaction' :: Transaction -> Maybe Address -- Signatures can be malformed, hence the Maybe
whoSignedThisTransaction' t = 
    fmap pubKey2Address $ getPubKeyFromSignature' xSignature theHash
  where
    xSignature = ExtendedSignature (Haskoin.Signature (fromInteger $ transactionR t) (fromInteger $ transactionS t)) (0x1c == transactionV t)
    SHA theHash = partialTransactionHash t
    getPubKeyFromSignature' = getPubKeyFromSignature_fast
    partialTransactionHash = hash . rlpSerialize . partialRLPEncode

runAllTests::Maybe String->Maybe String->ContextM ()
runAllTests maybeFileName maybeTestName= do
  putWSTT $ fromJust . whoSignedThisTransaction'
  let theFiles =
        case maybeFileName of
          Nothing -> testFiles
          Just fileName -> [fileName]
    
  forM_ theFiles $ \theFileName -> do
      theFile <- liftIO $ BL.readFile theFileName
      liftIO $ putStrLn $ C.yellow $ "#### Running tests in file: " ++ theFileName
      runTestsInFile maybeTestName theFile

runTestsInFile::Maybe String->BL.ByteString->ContextM ()
runTestsInFile maybeTestName theFile = do

  case fmap fromJSON $ eitherDecode theFile::Either String (Result Tests) of
    Left err -> liftIO $ putStrLn err
    Right val ->
      case val of
        Error err' -> liftIO $ putStrLn err'
        Success tests -> runTests (filter ((matchName maybeTestName) . fst) (M.toList tests))
  where
    matchName::Maybe String->String->Bool
    matchName Nothing _ = True
    matchName (Just x1) x2 | x1 == x2 = True
    matchName _ _ = False