{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Vendor.Projection
  ( Vendor (..)
  , VendorEvent (..)
  , vendorProjection
  ) where

import Data.Aeson.TH
import SumTypes.TH

import Eventful

import Bank.Models.Vendor.Events
import Bank.Json

data Vendor =
  Vendor
  { vendorName :: Maybe String
  } deriving (Show, Eq)

deriveJSON (unPrefixLower "vendor") ''Vendor

constructSumType "VendorEvent" (defaultSumTypeOptions { sumTypeOptionsTagOptions = AppendTypeNameToTags }) vendorEvents

deriving instance Show VendorEvent
deriving instance Eq VendorEvent

-- take a Vendor, an Event and return a Vendor
handleVendorEvent :: Vendor -> VendorEvent -> Vendor
handleVendorEvent vendor (VendorCreatedVendorEvent (VendorCreated name)) = vendor { vendorName = Just name }
handleVendorEvent vendor (VendorCreationRejectedVendorEvent _) = vendor

-- Seed and Event Handler Pair
vendorProjection :: Projection Vendor VendorEvent
vendorProjection = Projection (Vendor Nothing) handleVendorEvent
