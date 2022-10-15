{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.ComposedClauses.ComposedClauses
  where

import Contracts.ShipmentAndDelivery.ShipmentLocationClauses
import Contracts.ShipmentAndDelivery.RiskOfLossClauses
import Contracts.ShipmentAndDelivery.Types
import Contracts.Types
import Engine.Engine
import Preprocessor.Preprocessor


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
