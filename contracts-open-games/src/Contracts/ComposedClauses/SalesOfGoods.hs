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

import Contracts.ForceMajeur (forceMajeurClause)
import Contracts.Insurance.Clauses
import Contracts.Payments.Payments (paymentSettlement)
import Contracts.ShipmentAndDelivery.Export
import Contracts.Termination.Clauses ()
import Contracts.Warranty.Clauses (warrantyCosts)

import OpenGames.Engine.Engine
import OpenGames.Preprocessor

-- This module describes a composed model of the sales of good clauses

-- NOTE We do not include inspection clauses, liability, force majeur, and termination clauses in the first version of the contract here.
-- This needs to be modelled in a different fashion as these costs are contingent on things happening or on decisions in the course of the contract.

-- We give one example for the damage as modelled in an external fashion (NOTE the moral hazard version in the corresponding clause file)
{-
-- | Sales of good contract
salesOfGoods seller buyer interestRate costFunction probabilityDistribution damageFunction warrantyCostFunction insuranceCostFunction = [opengame|

    inputs    : price, daysLate, location, warranty, insuredSum, periodOfTimeInsurance ;
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
    operation : riskOfLoss seller buyer probabilityDistribution damageFunction ;
    outputs   : lossSeller,lossBuyer;
    returns   : ;

    inputs    : warranty ;
    feedback  : ;
    operation : warrantyCosts seller buyer warrantyCostFunction ;
    outputs   : ;
    returns   : ;

    inputs    : insuredSum, periodOfTimeInsurance ;
    feedback  : ;
    operation : insuranceClause seller buyer insuranceCostFunction;
    outputs   : ;
    returns   : ;

    :-----:

    outputs   : ;
    returns   : ;

\|]

-----------------------------------
-- Scenarios in the course of the
-- contract life-time
-----------------------------------

-- | What if a force majeur happened?
salesOfGoodsForceMajeur seller buyer interestRate costFunction probabilityDistribution damageFunction warrantyCostFunction insuranceCostFunction forceMajeurCostFunction = [opengame|

    inputs    : price, daysLate, location, warranty, insuredSum, periodOfTimeInsurance, eventHappened ;
    feedback  : ;

    :-----:

    inputs    : price, daysLate, location, warranty, insuredSum, periodOfTimeInsurance ;
    feedback  : ;
    operation : salesOfGoods  seller buyer interestRate costFunction probabilityDistribution damageFunction warrantyCostFunction insuranceCostFunction;
    outputs   : ;
    returns   : ;

    inputs    : eventHappened ;
    feedback  : ;
    operation : forceMajeurClause seller buyer forceMajeurCostFunction ;
    outputs   : ;
    returns   : ;

    :-----:

    outputs   : ;
    returns   : ;

\|]

-- | What if the inspection scenario kicks in?
-- We consider this a strategic choice
salesOfGoodsInspection seller buyer interestRate costFunction probabilityDistribution damageFunction warrantyCostFunction insuranceCostFunction inspectionCondition daysThreshold replacementCostFunction= [opengame|

    inputs    : price, daysLate, location, warranty, insuredSum, periodOfTimeInsurance, daysSinceShipment ;
    feedback  : ;

    :-----:

    inputs    : price, daysLate, location, warranty, insuredSum, periodOfTimeInsurance ;
    feedback  : ;
    operation : salesOfGoods  seller buyer interestRate costFunction probabilityDistribution damageFunction warrantyCostFunction insuranceCostFunction;
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

\|]

--}
