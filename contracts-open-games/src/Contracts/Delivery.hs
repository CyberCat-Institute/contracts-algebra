{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.Delivery where

import Engine.Engine
import Preprocessor.Preprocessor

import Data.Tuple.Extra (uncurry3)

-- Delivery and Shipment clauses
-- This module describes clauses where delivery and shipment clauses are made


---------------------------------------
-- 0 Data types and Auxiliary Functions
---------------------------------------

data Location = SellerLocation | BuyerLocation
  deriving (Show,Eq,Ord)

data Inspection = SellerInspection | BuyerInspection
  deriving (Show,Eq,Ord)

type BuyerInspected = Bool

type SellerInspectionConfirmed = Bool

type DaysUntilInspection = Int
type DaysUntilInspectionThreshold = Int

type Costs = Double
type SellerCosts = Costs
type BuyerCosts  = Costs

-- | InspectionCondition
inspectionOutcome :: DaysUntilInspectionThreshold  ->  Inspection -> DaysUntilInspection ->  BuyerInspected -> SellerInspectionConfirmed -> Bool
inspectionOutcome daysThreshold inspectionCondition days buyerDec sellerDec
   | daysThreshold < days = False
   | daysThreshold >= days && inspectionCondition == BuyerInspection && buyerDec == True = True
   | daysThreshold >= days && inspectionCondition == SellerInspection && buyerDec == True && sellerDec == True = True
   | otherwise = False 

-- | Shipment cost functions for specializing contracts
-- Assumes only one side bears the costs
shipmentCostFunction :: Costs -> Location -> (SellerCosts,BuyerCosts)
shipmentCostFunction c SellerLocation = (0,-c)
-- ^ When the seller is the location, the buyer bears the costs
shipmentCostFunction c BuyerLocation  = (-c,0)
-- ^ When the buyer is the location, the seller bears the costs

--------------------
-- 1 Shipping clauses
--------------------
-- | Delivery and shipment costs
-- Generic case
shipmentCosts seller buyer costFunction= [opengame|

    inputs    : location ;
    feedback  : ;

    :-----:

    inputs    : location ;
    feedback  : ;
    operation : forwardFunction  costFunction;
    outputs   : costsSeller,costsBuyer ;
    returns   : ;


    inputs    : costsSeller;
    feedback  : ;
    operation : addPayoffs seller ;
    outputs   :  ;
    returns   : ;


    inputs    : costsBuyer ;
    feedback  : ;
    operation : addPayoffs buyer ;
    outputs   :  ;
    returns   :  ;

    :-----:

    outputs   : ;
    returns   : ;
|]

-- | The location is fed as an exogenous parameter
shipmentCostsExogenousLocation seller buyer loc costs = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    inputs    : loc ; 
    feedback  : ;
    operation : shipmentCosts seller buyer (shipmentCostFunction costs) ;
    outputs   : ;
    returns   : ;

    :-----:

    outputs   : ;
    returns   : ;
|]

-- | Specialize to the case where the seller is the location where the goods are delivered
-- We fix arbitrary costs here.
shipmentCostsShipperLocation seller buyer = shipmentCostsExogenousLocation seller buyer SellerLocation 50


-- | Specialize to the case where the buyer/purchaser is the location where the goods are delivered
-- We fix arbitrary costs here.
shipmentCostsPurchaserLocation seller buyer = shipmentCostsExogenousLocation seller buyer BuyerLocation 50



-- | Generic risk of loss and distribution of costs
riskOfLoss seller buyer probabilityDistribution damageFunction= [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    inputs    : ;
    feedback  : ;
    operation : liftStochasticForward probabilityDistribution ;
    outputs   : isLost ;
    returns   : ;

    inputs    : isLost ;
    feedback  : ;
    operation : forwardFunction damageFunction;
    outputs   : costsSeller,costsBuyer ;
    returns   : ;

    inputs    : costsSeller;
    feedback  : ;
    operation : addPayoffs seller ;
    outputs   :  ;
    returns   : ;


    inputs    : costsBuyer ;
    feedback  : ;
    operation : addPayoffs buyer ;
    outputs   :  ;
    returns   :  ;

    :-----:

    outputs   : ;
    returns   : ;
|]

-- | Inspection decision; whether goods are non-confirming depends on the _inspectionCondition_
inspectionDecision seller buyer inspectionCondition daysThreshold = [opengame|

    inputs    : daysSinceShipment ;
    feedback  : ;

    :-----:
    inputs    : daysSinceShipment ;
    feedback  : ;
    operation : dependentDecision buyer $ const [True,False] ;
    outputs   : inspectedBuyer ;
    returns   : costsBuyer;

    inputs    : daysSinceShipment, inspectedBuyer ;
    feedback  : ;
    operation : dependentDecision seller $ const [True,False] ;
    outputs   : confirmedSeller ;
    returns   : costsSeller;

    inputs    : daysSinceShipment, inspectedBuyer,confirmedSeller ;
    feedback  : ;
    operation : forwardFunction $ uncurry3 $ inspectionOutcome inspectionCondition daysThreshold ;
    outputs   : nonConfirming ;
    returns   : ;

    :-----:

    outputs   : nonConfirming ;
    returns   : costsSeller,costsBuyer;

|]

  
-- | Determines the costs for buyers and sellers when a good is non-confirming
inspectionConsequences seller buyer replacementCostFunction = [opengame|

    inputs    : nonConfirming ;
    feedback  : ;

    :-----:

     inputs    : nonConfirming ;
    feedback  : ;
    operation : forwardFunction replacementCostFunction;
    outputs   : costsSeller,costsBuyer ;
    returns   : ;

    :-----:

    outputs   :costsSeller,costsBuyer ;
    returns   : ;

|]
