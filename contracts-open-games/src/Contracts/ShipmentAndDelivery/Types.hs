module Contracts.ShipmentAndDelivery.Types where

---------------
-- 0 Data types
---------------

data Location = SellerLocation | BuyerLocation
  deriving (Show,Eq,Ord)

type Costs = Double
type SellerCosts = Costs
type BuyerCosts  = Costs

data ModeInspection = SellerInspection | BuyerInspection
  deriving (Show,Eq,Ord)

type BuyerInspected = Bool

type SellerInspectionConfirmed = Bool

type DaysUntilInspection = Int
type DaysUntilInspectionThreshold = Int

data ModePackagingShipping = BuyerFavorablePackagingShipping | NeutralPackagingShipping | SellerFavorablePackagingShipping | SellerVeryFavorablePackagingShipping
  deriving (Show,Eq,Ord)

data ModeRiskOfLoss = BuyerRisk | NeutralRisk | SellerRisk
  deriving (Show,Eq,Ord)

