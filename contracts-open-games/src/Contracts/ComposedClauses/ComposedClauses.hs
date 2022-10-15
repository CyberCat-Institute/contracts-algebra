{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.ComposedClauses.ComposedClauses
  where

import Contracts.ShipmentAndDelivery.InspectionClauseDecisions
import Contracts.ShipmentAndDelivery.ShipmentLocationClauses
import Contracts.ShipmentAndDelivery.RiskOfLossClauses
import Contracts.ShipmentAndDelivery.Types
import Contracts.Warranty.Clauses
import Contracts.Types
import Engine.Engine
import Preprocessor.Preprocessor


-----------------------------
-- 0. Auxiliary functionality
----------------------------

warrantyApplies :: (Bool,Costs) -> Costs
warrantyApplies (True,c)  = c
warrantyApplies (False,_) = 0

----------------------
-- 1. Composed clauses
----------------------

-- | Compose riskOfLoss contract and shipmentCosts clauses
locationPlusRiskOfLoss seller buyer damageFunction mode costFunction = [opengame|

    inputs    : damage, location, buyerReceived ;
    feedback  : ;

    :-----:

    inputs    : damage, location, buyerReceived;
    feedback  : ;
    operation : riskOfLoss seller buyer damageFunction mode ;
    outputs   : damageSeller,damageBuyer;
    returns   : ;


    inputs    : location ;
    feedback  : ;
    operation : shipmentCosts seller buyer costFunction ;
    outputs   : costsSeller,costsBuyer;
    returns   :  ;

    :-----:

    outputs   : ;
    returns   : ;
|]

-- | Compose warranty and  
inspectionPlusWarranty seller buyer  daysThreshold inspectionCondition warrantyCostFunction warrantyAffected = [opengame|

    inputs    : daysSinceShipment, warranty ;
    feedback  : ;

    :-----:

    inputs    : daysSinceShipment;
    feedback  : ;
    operation : inspectionDecision seller buyer  daysThreshold inspectionCondition ;
    outputs   : nonConfirming;
    returns   : costsSeller,costsBuyer ;

    inputs    : nonConfirming, warranty;
    feedback  : ;
    operation : forwardFunction $ warrantyApplies ;
    outputs   : warrantyRealized;
    returns   : ;

   

    inputs    : warrantyRealized ;
    feedback  : ;
    operation : warrantyCosts seller buyer warrantyCostFunction ;
    outputs   : costsSeller,costsBuyer;
    returns   :  ;

    :-----:

    outputs   : ;
    returns   : ;
|]
