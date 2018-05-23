{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Vendor.Events
  ( vendorEvents
  , VendorCreated (..)
  , VendorCreationRejected (..)
  ) where

import Language.Haskell.TH (Name)

import Bank.Json

vendorEvents :: [Name]
vendorEvents =
  [ ''VendorCreated
  , ''VendorCreationRejected
  ]

data VendorCreated =
  VendorCreated
  { vendorCreatedName :: String
  } deriving (Show, Eq)

data VendorCreationRejected
  = VendorCreationRejected
  { vendorCreationRejectedReason :: String
  } deriving (Show, Eq)

deriveJSONUnPrefixLower ''VendorCreated
deriveJSONUnPrefixLower ''VendorCreationRejected
