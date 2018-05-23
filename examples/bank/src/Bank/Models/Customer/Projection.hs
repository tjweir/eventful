{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Customer.Projection
  ( Customer (..)
  , CustomerEvent (..)
  , customerProjection
  ) where

import Data.Aeson.TH
import SumTypes.TH

import Eventful

import Bank.Models.Customer.Events
import Bank.Json

type CustomerUUID = UUID

data Customer =
  Customer
  { customerName :: Maybe String
  , customerLocation :: Maybe String
  } deriving (Show, Eq)

deriveJSON (unPrefixLower "customer") ''Customer

constructSumType "CustomerEvent" (defaultSumTypeOptions { sumTypeOptionsTagOptions = AppendTypeNameToTags }) customerEvents

deriving instance Show CustomerEvent
deriving instance Eq CustomerEvent

handleCustomerEvent :: Customer -> CustomerEvent -> Customer
handleCustomerEvent customer (CustomerCreatedCustomerEvent (CustomerCreated name location)) = customer { customerName = Just name }
handleCustomerEvent customer (CustomerCreationRejectedCustomerEvent _) = customer
handleCustomerEvent customer (CustomerUpdatedCustomerEvent (CustomerUpdated name location)) = customer { customerName = Just name }
handleCustomerEvent customer (CustomerUpdateRejectedCustomerEvent _) = customer

customerProjection :: Projection Customer CustomerEvent
customerProjection = Projection (Customer Nothing Nothing) handleCustomerEvent
