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

-- | InspectionCondition
inspectionOutcome :: Inspection ->  BuyerInspected -> SellerInspectionConfirmed -> Bool
inspectionOutcome BuyerInspection  True  _ = True
inspectionOutcome SellerInspection True  True = True
inspectionOutcome _                _     _    = False

--------------------
-- 1 Shipping clauses
--------------------
-- | Delivery and shipment costs
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

-- | Risk of loss and distribution of costs
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



inspectionConsequences seller buyer inspectionCondition replacementCostFunction = [opengame|

    inputs    : inspectedBuyer,confirmedSeller ;
    feedback  : ;

    :-----:

    inputs    : inspectedBuyer,confirmedSeller ;
    feedback  : ;
    operation : forwardFunction $ uncurry $ inspectionOutcome inspectionCondition ;
    outputs   : nonConfirming ;
    returns   : ;

    inputs    : nonConfirming ;
    feedback  : ;
    operation : forwardFunction replacementCostFunction;
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
