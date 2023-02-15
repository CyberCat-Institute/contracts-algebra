{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.ShipmentAndDelivery.InspectionClauseDecisions where

import Contracts.ShipmentAndDelivery.Types
import Contracts.Types
import OpenGames.Engine.Engine
import OpenGames.Preprocessor


import Data.Tuple.Extra (uncurry3)

-- This module describes clauses where inspection decisions are made
-- This describes the execution phase of a given contract


---------------------------------------
-- 0 Data types and Auxiliary Functions
---------------------------------------

-- | InspectionCondition
inspectionOutcome :: DaysUntilInspectionThreshold  ->  Player -> DaysUntilInspection ->  BuyerInspected -> SellerInspectionConfirmed -> Bool
inspectionOutcome daysThreshold inspectionCondition days buyerDec sellerDec
   | daysThreshold < days = False
   | daysThreshold >= days && inspectionCondition == Buyer && buyerDec == True = True
   | daysThreshold >= days && inspectionCondition == Seller && buyerDec == True && sellerDec == True = True
   | otherwise = False 

-- | Replacement costs for buyers and sellers
-- We assume no costs for the buyer; could be different depending on the situation
replacementCostsFunctionSellerOnly :: Costs -> Bool -> (Costs,Costs)
replacementCostsFunctionSellerOnly _     True  = (0,0)
replacementCostsFunctionSellerOnly costs False = (-costs,0)

-----------------------------
-- 1 Shipping strategic games
-----------------------------
-- | Inspection decision; whether goods are non-confirming depends on the _inspectionCondition_
inspectionDecision seller buyer  daysThreshold inspectionCondition = [opengame|

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
inspectionConsequences
  :: Show a =>
     p1
     -> p2
     -> (a -> (b, c))
     -> OpenGame
          StochasticStatefulOptic
          StochasticStatefulContext
          '[]
          '[]
          a
          ()
          (b, c)
          ()
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

-- | Complete game
inspectionGame seller buyer  daysThreshold replacementCostFunction inspectionCondition= [opengame|

    inputs    : daysSinceShipment;
    feedback  : ;

    :-----:

    inputs    : daysSinceShipment ;
    feedback  : ;
    operation : inspectionDecision seller buyer inspectionCondition daysThreshold;
    outputs   : nonConfirming ;
    returns   : costsSeller,costsBuyer ;

    inputs    : nonConfirming ;
    feedback  : ;
    operation : inspectionConsequences seller buyer replacementCostFunction;
    outputs   : costsSeller,costsBuyer ;
    returns   :  ;

    :-----:

    outputs   : costsSeller,costsBuyer;
    returns   : ;

|]

-- | Complete game
inspectionGameExogenous seller buyer daysThreshold daysSinceShipment replacementCostFunction inspectionCondition = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    inputs    : daysSinceShipment ;
    feedback  : ;
    operation : inspectionGame seller buyer daysThreshold replacementCostFunction inspectionCondition;
    outputs   : costsSeller,costsBuyer;
    returns   : ;

    :-----:

    outputs   : costsSeller,costsBuyer;
    returns   : ;

|]


-- | Specialize to inspection mode as well as above costfunction
inspectionBuyer seller buyer  daysThreshold daysSinceShipment costs = inspectionGameExogenous seller buyer  daysThreshold daysSinceShipment (replacementCostsFunctionSellerOnly costs) Buyer
inspectionSeller seller buyer  daysThreshold daysSinceShipment costs = inspectionGameExogenous seller buyer  daysThreshold daysSinceShipment (replacementCostsFunctionSellerOnly costs) Seller

