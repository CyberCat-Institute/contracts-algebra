{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.ShipmentAndDelivery.PackagingShippingClauses where

import Contracts.ShipmentAndDelivery.Types
import Contracts.Types
import Engine.Engine
import Preprocessor.Preprocessor

-- Packaging and Shipping Clauses
-- This module describes clauses where delivery and shipment clauses are made

------------------------
-- 0 Auxiliary Functions
------------------------
-- | Shipment and packaging cost functions for specializing contracts
-- Assumes that the buyer will require higher costs; otherwise normalized to 0
packagingCostFunction :: Costs -> Costs -> Costs -> Costs -> ModePackagingShipping ->  (SellerCosts,BuyerCosts)
packagingCostFunction costsShippingHigh costsShippingLow costsPackagingHigh costsPackagingLow BuyerFavorablePackagingShipping =
  (-costsShippingHigh -costsPackagingHigh,0)
packagingCostFunction costsShippingHigh costsShippingLow costsPackagingHigh costsPackagingLow NeutralPackagingShipping =
  (-costsShippingHigh -costsPackagingLow,0)
packagingCostFunction costsShippingHigh costsShippingLow costsPackagingHigh costsPackagingLow SellerFavorablePackagingShipping =
  (-costsShippingLow -costsPackagingLow,0)
packagingCostFunction costsShippingHigh costsShippingLow costsPackagingHigh costsPackagingLow SellerVeryFavorablePackagingShipping =
  (-costsShippingLow ,-costsPackagingHigh)


-----------------------------------
-- 1 Shipping and packaging clauses
-----------------------------------
-- | Delivery and shipment costs
-- Generic case
shipmentPackagingCosts seller buyer costsShippingHigh costsShippingLow costsPackagingHigh costsPackagingLow = [opengame|

    inputs    : mode ;
    feedback  : ;

    :-----:

    inputs    : mode ;
    feedback  : ;
    operation : forwardFunction $ packagingCostFunction costsShippingHigh costsShippingLow costsPackagingHigh costsPackagingLow;
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

  
-- | The shipment packaging mode is fed as an exogenous parameter
shipmentPackagingCostsExogenousLocation seller buyer costsShippingHigh costsShippingLow costsPackagingHigh costsPackagingLow mode  = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    inputs    : mode ; 
    feedback  : ;
    operation : shipmentPackagingCosts seller buyer costsShippingHigh costsShippingLow costsPackagingHigh costsPackagingLow ;
    outputs   : costsSeller,costsBuyer;
    returns   : ;

    :-----:

    outputs   : costsSeller,costsBuyer;
    returns   : ;
|]

-- | Specialize to the different modes of packaging and shipping
shipmentPackagingCostsBuyerFavorable seller buyer  costsShippingHigh costsShippingLow costsPackagingHigh costsPackagingLow= shipmentPackagingCostsExogenousLocation seller buyer costsShippingHigh costsShippingLow costsPackagingHigh costsPackagingLow BuyerFavorablePackagingShipping

shipmentPackagingCostsNeutral seller buyer  costsShippingHigh costsShippingLow costsPackagingHigh costsPackagingLow= shipmentPackagingCostsExogenousLocation seller buyer costsShippingHigh costsShippingLow costsPackagingHigh costsPackagingLow NeutralPackagingShipping

shipmentPackagingCostsSellerFavorable seller buyer  costsShippingHigh costsShippingLow costsPackagingHigh costsPackagingLow= shipmentPackagingCostsExogenousLocation seller buyer costsShippingHigh costsShippingLow costsPackagingHigh costsPackagingLow SellerFavorablePackagingShipping

shipmentPackagingCostsSellerVeryFavorable seller buyer  costsShippingHigh costsShippingLow costsPackagingHigh costsPackagingLow= shipmentPackagingCostsExogenousLocation seller buyer costsShippingHigh costsShippingLow costsPackagingHigh costsPackagingLow SellerVeryFavorablePackagingShipping





