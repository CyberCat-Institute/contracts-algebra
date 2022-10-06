{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.ShipmentAndDelivery.ShipmentLocationClauses where

import Contracts.ShipmentAndDelivery.Types
import Contracts.Types
import Engine.Engine
import Preprocessor.Preprocessor



-- Shipment location clauses

------------------------
-- 0 Auxiliary Functions
------------------------

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

    outputs   : costsSeller,costsBuyer;
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
    outputs   : costsSeller,costsBuyer;
    returns   : ;

    :-----:

    outputs   : costsSeller,costsBuyer;
    returns   : ;
|]

-- | Specialize to the case where the seller is the location where the goods are delivered
-- We fix arbitrary costs here.
shipmentCostsShipperLocation seller buyer costs = shipmentCostsExogenousLocation seller buyer SellerLocation costs


-- | Specialize to the case where the buyer/purchaser is the location where the goods are delivered
-- We fix arbitrary costs here.
shipmentCostsPurchaserLocation seller buyer costs = shipmentCostsExogenousLocation seller buyer BuyerLocation costs



