{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Customer.Events
  ( customerEvents
  , CustomerCreated (..)
  , CustomerCreationRejected (..)
  , CustomerUpdated (..)
  , CustomerUpdateRejected (..)
  ) where

import Language.Haskell.TH (Name)

import Bank.Json

customerEvents :: [Name]
customerEvents =
  [ ''CustomerCreated
  , ''CustomerCreationRejected
  , ''CustomerUpdated
  , ''CustomerUpdateRejected
  ]

data CustomerCreated =
  CustomerCreated
  { customerCreatedName :: String
  } deriving (Show, Eq)

data CustomerCreationRejected
  = CustomerCreationRejected
  { customerCreationRejectedReason :: String
  } deriving (Show, Eq)

data CustomerUpdated =
  CustomerUpdated
  { customerUpdatedName :: String
  } deriving (Show, Eq)

data CustomerUpdateRejected
  = CustomerUpdateRejected
  { customerUpdateRejectedReason :: String
  } deriving (Show, Eq)


deriveJSONUnPrefixLower ''CustomerCreated
deriveJSONUnPrefixLower ''CustomerCreationRejected
deriveJSONUnPrefixLower ''CustomerUpdated
deriveJSONUnPrefixLower ''CustomerUpdateRejected

