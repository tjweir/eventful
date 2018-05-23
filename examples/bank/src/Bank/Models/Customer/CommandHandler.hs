{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Customer.CommandHandler
  ( CustomerCommand (..)
  , customerCommandHandler
  ) where

import SumTypes.TH
import Eventful

import Bank.Models.Customer.Commands
import Bank.Models.Customer.Events
import Bank.Models.Customer.Projection

constructSumType "CustomerCommand" (defaultSumTypeOptions { sumTypeOptionsTagOptions = AppendTypeNameToTags }) customerCommands

handleCustomerCommand :: Customer -> CustomerCommand -> [CustomerEvent]
handleCustomerCommand customer (CreateCustomerCustomerCommand (CreateCustomer name location)) =
  case customerName customer of
    Nothing -> [CustomerCreatedCustomerEvent $ CustomerCreated name location]
    Just _  -> [CustomerCreationRejectedCustomerEvent $ CustomerCreationRejected "Customer already exists."]
handleCustomerCommand customer (UpdateCustomerCustomerCommand (UpdateCustomer uuid name location)) =
  case customerName customer of
    Just _  -> [CustomerUpdatedCustomerEvent $ (CustomerUpdated name location)]
    Nothing -> [CustomerUpdateRejectedCustomerEvent $ CustomerUpdateRejected "Customer does not exist."]

  
customerCommandHandler :: CommandHandler Customer CustomerEvent CustomerCommand
customerCommandHandler  = CommandHandler handleCustomerCommand customerProjection
