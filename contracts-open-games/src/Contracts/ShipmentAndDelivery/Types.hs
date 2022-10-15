module Contracts.ShipmentAndDelivery.Types where

---------------
-- 0 Data types
---------------
data Player = Seller | Buyer
  deriving (Show,Eq,Ord)

type BuyerInspected = Bool

type SellerInspectionConfirmed = Bool

type BuyerReceived = Bool

type DaysUntilInspection = Int

type DaysUntilInspectionThreshold = Int

data ModePackagingShipping = BuyerFavorablePackagingShipping | NeutralPackagingShipping | SellerFavorablePackagingShipping | SellerVeryFavorablePackagingShipping
  deriving (Show,Eq,Ord)

data ModeOfRiskOfLoss = DeliveryLocation | HandOverToCarrier
  deriving (Show,Eq,Ord)
