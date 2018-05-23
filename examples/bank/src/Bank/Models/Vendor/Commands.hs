{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Vendor.Commands
  ( vendorCommands
  , CreateVendor (..)
  ) where

import Language.Haskell.TH (Name)

import Bank.Json

vendorCommands :: [Name]
vendorCommands =
  [ ''CreateVendor
  ]

data CreateVendor =
  CreateVendor
  { createVendorData :: String
  } deriving (Show, Eq)

deriveJSONUnPrefixLower ''CreateVendor
