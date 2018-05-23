{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Customer.Commands
  ( customerCommands
  , CreateCustomer (..)
  , UpdateCustomer (..)
  ) where

import Language.Haskell.TH (Name)

import Eventful.UUID

import Bank.Json

customerCommands :: [Name]
customerCommands =
  [ ''CreateCustomer
  , ''UpdateCustomer
  ]

data CreateCustomer =
  CreateCustomer
  { createCustomerData :: String
  , createCustomerLocation :: String
  } deriving (Show, Eq)

data UpdateCustomer =
  UpdateCustomer
  { updateCustomerUUID :: UUID
  , updateCustomerName :: String
  , updateCustomerLocation :: String
  } deriving (Show, Eq)


deriveJSONUnPrefixLower ''CreateCustomer
deriveJSONUnPrefixLower ''UpdateCustomer
