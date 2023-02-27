{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Contracts.ComposedClauses.SalesOfGoods where

import Contracts.ForceMajeur.Clauses (forceMajeurClause)
import Contracts.Insurance.Clauses
import Contracts.Payments.Payments (paymentSettlement)
import Contracts.ShipmentAndDelivery.Export
import Contracts.Termination.Clauses
import Contracts.Types
import Contracts.Warranty.Clauses (warrantyCosts)

import OpenGames.Engine.Engine
import OpenGames.Preprocessor

{-
-- This module describes an example for a composed model of the sales of good clauses;
-- It does not fully specify the whole contract but focuses on a subset of clauses
-- NOTE that we provide different foci - ex ante as well as in the course of the contract
-- NOTE that there are multiple ways in which these contracts could be used. There is a lot of space to be explored. We intentionally provide different viewpoints. 
-- TODO there are probably still some inconsistencies how we include the different contracts
-}


----------
-- Ex ante
----------

-- | Sales of good contract
salesOfGoods seller buyer interestRate costFunction costs damageFunction mode probabilityDistribution warrantyCostFunction insuranceCostFunction  = [opengame|

    inputs    : price, daysLate, location, warranty, insuranceCondition, amountSeller,amountBuyer, deadlineInsurance, timeOfInsurance ;
    feedback  : ;

    :-----:

    inputs    : price, daysLate ;
    feedback  : ;
    operation : paymentSettlement seller buyer interestRate ;
    outputs   : ;
    returns   : ;

    inputs    : location ;
    feedback  : ;
    operation : shipmentCosts seller buyer costFunction;
    outputs   : shipmentCostsSeller,shipmentCostsBuyer;
    returns   : ;

    inputs    : ;
    feedback  : ;
    operation : riskOfLossExpectationExogenous seller buyer costs damageFunction mode probabilityDistribution ;
    outputs   : costsLossSeller,costsLossBuyer;
    returns   : ;

    inputs    : warranty ;
    feedback  : ;
    operation : warrantyCosts seller buyer warrantyCostFunction ;
    outputs   : costsWarrantySeller,costsWarrantyBuyer;
    returns   : ;

    inputs    : insuranceCondition, amountSeller,amountBuyer, deadlineInsurance, timeOfInsurance ;
    feedback  : ;
    operation : insuranceClause seller buyer insuranceCostFunction ;
    outputs   : ;
    returns   : ;

    :-----:

    outputs   : ;
    returns   : ;

|]

-----------------------------------
-- Scenarios in the course of the
-- contract life-time
-----------------------------------

-- | What if a force majeur happened?
salesOfGoodsForceMajeur seller buyer interestRate costFunction costs damageFunction mode probabilityDistribution warrantyCostFunction insuranceCostFunction = [opengame|

    inputs    : price, daysLate, location, warranty, insuranceCondition, amountSeller, amountBuyer, deadlineInsurance, timeOfInsurance, eventHappened, forceMajeurCondition, deadlineForceMajeur, timeForceMajeur ;
    feedback  : ;

    :-----:

    inputs    : price, daysLate, location, warranty, insuranceCondition, amountSeller,amountBuyer, deadlineInsurance, timeOfInsurance ;
    feedback  : ;
    operation : salesOfGoods  seller buyer interestRate costFunction costs damageFunction mode probabilityDistribution warrantyCostFunction insuranceCostFunction;
    outputs   : ;
    returns   : ;

    inputs    : eventHappened, forceMajeurCondition, deadlineForceMajeur, timeForceMajeur ;
    feedback  : ;
    operation : forceMajeurClause seller buyer  ;
    outputs   : terminateContract;
    returns   : costTerminationSeller,costTerminationBuyer;

    :-----:

    outputs   : ;
    returns   : costTerminationSeller,costTerminationBuyer;

|]

-- | What if the inspection scenario kicks in?
-- We consider this a strategic choice
salesOfGoodsInspection seller buyer interestRate costFunction costs damageFunction mode probabilityDistribution warrantyCostFunction insuranceCostFunction inspectionCondition daysThreshold replacementCostFunction= [opengame|

    inputs    : price, daysLate, location, warranty, insuranceCondition, amountSeller,amountBuyer, deadlineInsurance, timeOfInsurance, daysSinceShipment ;
    feedback  : ;

    :-----:

    inputs    : price, daysLate, location, warranty, insuranceCondition, amountSeller,amountBuyer, deadlineInsurance, timeOfInsurance ;
    feedback  : ;
    operation : salesOfGoods  seller buyer interestRate costFunction costs damageFunction mode probabilityDistribution warrantyCostFunction insuranceCostFunction;
    outputs   : ;
    returns   : ;

    inputs    : daysSinceShipment ;
    feedback  : ;
    operation : inspectionDecision seller buyer inspectionCondition daysThreshold ;
    outputs   : nonConfirming;
    returns   : costsSeller,costsBuyer;

    inputs    : nonConfirming ;
    feedback  : ;
    operation : inspectionConsequences seller buyer replacementCostFunction ;
    outputs   : costsSeller,costsBuyer;
    returns   : ;

    :-----:

    outputs   : ;
    returns   : ;

|]

