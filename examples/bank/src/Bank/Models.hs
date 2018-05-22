{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Bank.Models
  ( BankEvent (..)
  , BankCommand (..)
  , accountEventSerializer
  , accountCommandSerializer
  , accountBankProjection
  , accountBankCommandHandler
  , customerEventSerializer
  , customerCommandSerializer
  , customerBankProjection
  , customerBankCommandHandler
  , vendorEventSerializer
  , vendorCommandSerializer
  , vendorBankProjection
  , vendorBankCommandHandler
  , module X
  ) where

import Data.Aeson.TH
import SumTypes.TH

import Eventful
import Eventful.TH

import Bank.Json
import Bank.Models.Account as X
import Bank.Models.Customer as X
import Bank.Models.Vendor as X

constructSumType "BankEvent" (defaultSumTypeOptions { sumTypeOptionsTagOptions = ConstructTagName (++ "Event") }) $
  concat
  [ accountEvents
  , customerEvents
  , vendorEvents
  ]

deriving instance Show BankEvent
deriving instance Eq BankEvent

deriveJSON (defaultOptions { constructorTagModifier = dropSuffix "Event" }) ''BankEvent

constructSumType "BankCommand" (defaultSumTypeOptions { sumTypeOptionsTagOptions = ConstructTagName (++ "Command") }) $
  concat
  [ accountCommands
  , customerCommands
  , vendorCommands
  ]

deriving instance Show BankCommand
deriving instance Eq BankCommand


-- Accounts
mkSumTypeSerializer "accountEventSerializer" ''AccountEvent ''BankEvent
mkSumTypeSerializer "accountCommandSerializer" ''AccountCommand ''BankCommand

accountBankProjection :: Projection Account BankEvent
accountBankProjection = serializedProjection accountProjection accountEventSerializer

accountBankCommandHandler :: CommandHandler Account BankEvent BankCommand
accountBankCommandHandler = serializedCommandHandler accountCommandHandler accountEventSerializer accountCommandSerializer

-- Customers
mkSumTypeSerializer "customerEventSerializer" ''CustomerEvent ''BankEvent
mkSumTypeSerializer "customerCommandSerializer" ''CustomerCommand ''BankCommand

customerBankProjection :: Projection Customer BankEvent
customerBankProjection = serializedProjection customerProjection customerEventSerializer

customerBankCommandHandler :: CommandHandler Customer BankEvent BankCommand
customerBankCommandHandler = serializedCommandHandler customerCommandHandler customerEventSerializer customerCommandSerializer

-- Vendors
-- Merge VendorEvents to Bank Events  
mkSumTypeSerializer "vendorEventSerializer" ''VendorEvent ''BankEvent
-- Merge VendorEvents to Bank Events  
mkSumTypeSerializer "vendorCommandSerializer" ''VendorCommand ''BankCommand

vendorBankProjection :: Projection Vendor BankEvent
vendorBankProjection = serializedProjection vendorProjection vendorEventSerializer

vendorBankCommandHandler :: CommandHandler Vendor BankEvent BankCommand
vendorBankCommandHandler = serializedCommandHandler vendorCommandHandler vendorEventSerializer vendorCommandSerializer
