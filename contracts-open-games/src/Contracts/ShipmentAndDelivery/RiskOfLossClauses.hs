{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.ShipmentAndDelivery.RiskOfLossClauses where

import Engine.Engine
import Preprocessor.Preprocessor


-- This module describes clauses which touch on risk of loss


-----------------------------
-- 1 Shipping strategic games
-----------------------------
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
