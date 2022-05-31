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


---------------
-- 0 Data types
---------------

data Location = SellerLocation | BuyerLocation
  deriving (Show,Eq,Ord)


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
