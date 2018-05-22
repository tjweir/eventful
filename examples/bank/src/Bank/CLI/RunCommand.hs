module Bank.CLI.RunCommand
  ( runCLICommand
  ) where

import Control.Monad (void)
import Database.Persist.Sqlite

import Eventful

import Bank.Models
import Bank.CLI.Options
import Bank.CLI.Store
import Bank.ReadModels.CustomerAccounts

runCLICommand :: ConnectionPool -> CLICommand -> IO ()
runCLICommand pool (CreateCustomerCLI createCommand) = do
  uuid <- uuidNextRandom
  putStr "Attempting to create customer with UUID: "
  print uuid
  let command = CreateCustomerCommand createCommand
  void $ runDB pool $ applyCommandHandler cliEventStoreWriter cliEventStoreReader customerBankCommandHandler uuid command

runCLICommand pool (UpdateCustomerCLI uuid name) = do
  putStr "Attempting to update customer with UUID: "
  print uuid
  print name
  let command = UpdateCustomerCommand $ UpdateCustomer uuid name
  void $ runDB pool $ applyCommandHandler cliEventStoreWriter cliEventStoreReader customerBankCommandHandler uuid command

runCLICommand pool (ViewAccountCLI uuid) = do
  putStr "Viewing Account details for UUID: "
  print uuid
  latestStreamProjection <- runDB pool $
    getLatestStreamProjection cliEventStoreReader (versionedStreamProjection uuid accountBankProjection)
  printJSONPretty (streamProjectionState latestStreamProjection)

runCLICommand pool (ViewCustomerCLI uuid) = do
  putStr "Viewing Customer details for UUID: "
  print uuid
  latestStreamProjection <- runDB pool $
    getLatestStreamProjection cliEventStoreReader (versionedStreamProjection uuid customerBankProjection)
  printJSONPretty (streamProjectionState latestStreamProjection)

runCLICommand pool (ViewCustomerAccountsCLI name) = do
  events <- runDB pool $ getEvents cliGlobalEventStoreReader (allEvents ())
  let
    allCustomerAccounts = latestProjection customerAccountsProjection (streamEventEvent <$> events)
    thisCustomerAccounts = getCustomerAccountsFromName allCustomerAccounts name
  case thisCustomerAccounts of
    [] -> putStrLn "No accounts found"
    accounts -> mapM_ printJSONPretty accounts

runCLICommand pool (OpenAccountCLI openCommand) = do
  uuid <- uuidNextRandom
  putStr "Attempting to open account with UUID: "
  print uuid
  let command = OpenAccountCommand openCommand
  void $ runDB pool $ applyCommandHandler cliEventStoreWriter cliEventStoreReader accountBankCommandHandler uuid command

runCLICommand pool (TransferToAccountCLI sourceId amount targetId) = do
  putStrLn $ "Starting transfer from acccount " ++ show sourceId ++ " to " ++ show targetId
  transferId <- uuidNextRandom
  let startCommand = TransferToAccountCommand $ TransferToAccount transferId amount targetId
  void $ runDB pool $ applyCommandHandler cliEventStoreWriter cliEventStoreReader accountBankCommandHandler sourceId startCommand
  runCLICommand pool (ViewAccountCLI sourceId)
  runCLICommand pool (ViewAccountCLI targetId)

runCLICommand pool (CreateVendorCLI createVendor) = do
  uuid <- uuidNextRandom
  putStr "Attempting to create vendor with UUID: "
  print uuid
  let command = CreateVendorCommand createVendor
  void $ runDB pool $ applyCommandHandler cliEventStoreWriter cliEventStoreReader vendorBankCommandHandler uuid command



