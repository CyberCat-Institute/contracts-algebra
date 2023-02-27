module Contracts.Types
  where

-- Module with types shared across different clauses and contracts

type Costs = Double

type SellerCosts = Costs

type BuyerCosts  = Costs

type CurrentDate = Int

data TerminateContract = Continue | Terminate
  deriving (Show, Eq, Ord)

