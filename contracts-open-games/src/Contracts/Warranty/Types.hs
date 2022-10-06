module Contracts.Warranty.Types
  where

data Warranty = NoWarranty | LimitedWarranty | FullWarranty
  deriving (Show,Eq,Ord)

type ShareOfWarranty = Double

