{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Vendor.CommandHandler
  ( VendorCommand (..)
  , vendorCommandHandler
  ) where

import SumTypes.TH

import Eventful

import Bank.Models.Vendor.Commands
import Bank.Models.Vendor.Events
import Bank.Models.Vendor.Projection

constructSumType "VendorCommand" (defaultSumTypeOptions { sumTypeOptionsTagOptions = AppendTypeNameToTags }) vendorCommands

handleVendorCommand :: Vendor -> VendorCommand -> [VendorEvent]
handleVendorCommand vendor (CreateVendorVendorCommand (CreateVendor name)) =
  case vendorName vendor of
    Nothing -> [VendorCreatedVendorEvent $ VendorCreated name]
    Just _  -> [VendorCreationRejectedVendorEvent $ VendorCreationRejected "Vendor already exists"]

vendorCommandHandler :: CommandHandler Vendor VendorEvent VendorCommand
vendorCommandHandler = CommandHandler handleVendorCommand vendorProjection
