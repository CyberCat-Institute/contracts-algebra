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

import Contracts.ShipmentAndDelivery.Types


-- This module describes clauses which touch on risk of loss

------------------------
-- 0 Auxiliary Functions
------------------------
-- | How is possible damage distributed?
riskOfLossFunction :: ModeRiskOfLoss -> Costs ->  (SellerCosts,BuyerCosts)
riskOfLossFunction mode costs
  | mode == BuyerRisk   = (0,-costs)
  | mode == NeutralRisk = (-lossShared,-lossShared)
  | mode == SellerRisk  = (-lossShared,-lossShared)
 where lossShared = costs/2


----------------------
-- 1 RiskOfLossClauses
----------------------
-- | Generic risk of loss distribution
riskOfLoss seller buyer damageFunction  mode= [opengame|

    inputs    : damage ;
    feedback  : ;

    :-----:

    inputs    : damage ;
    feedback  : ;
    operation : forwardFunction $ damageFunction mode;
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


-- | Damage is fed as an exogenous parameter
riskOfLossExogenous seller buyer damageFunction damage mode= [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    inputs    : damage ;
    feedback  : ;
    operation : riskOfLoss seller buyer damageFunction mode ;
    outputs   : ;
    returns   : ;

    :-----:

    outputs   : ;
    returns   : ;
|]


  
-- | Specialize to above cost function and fix probabilities
riskOfLossParameterized seller buyer damage mode = riskOfLossExogenous seller buyer riskOfLossFunction damage mode

riskOfLossBuyer seller buyer damage = riskOfLossParameterized seller buyer damage BuyerRisk
riskOfLossNeutral seller buyer damage = riskOfLossParameterized seller buyer damage NeutralRisk
riskOfLossSeller seller buyer damage = riskOfLossParameterized seller buyer damage SellerRisk


----------------------------------
-- 2 RiskOfLoss Clauses -- ex ante
----------------------------------

-- | Generic risk of loss and distribution of costs
riskOfLossExpectation
  :: (Show b, Show r) =>
     String
     -> String
     -> b
     -> (t -> r -> (Payoff, Payoff))
     -> t
     -> (b -> Stochastic r)
     -> OpenGame
          StochasticStatefulOptic
          StochasticStatefulContext
          '[]
          '[]
          ()
          ()
          ()
          ()
riskOfLossExpectation seller buyer costs damageFunction mode probabilityDistribution = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    inputs    : costs ;
    feedback  : ;
    operation : liftStochasticForward probabilityDistribution;
    outputs   : damage;
    returns   : ;

    inputs    : damage;
    feedback  : ;
    operation : riskOfLoss seller buyer damageFunction mode;
    outputs   : ;
    returns   : ;

    :-----:

    outputs   : ;
    returns   : ;
|]

-- | Specialize to above cost function and fix probabilities
riskOfLossParameterizedExpectation seller buyer damage probDamage mode= seller buyer damage damageFunction mode probabilityDistribution
  where probabilityDistribution damage = distFromList [(probDamage,damage),(1-probDamage,0)]
        damageFunction = riskOfLossFunction

riskOfLossExpectationBuyer seller buyer damage probDamage = riskOfLossParameterizedExpectation seller buyer damage probDamage BuyerRisk
riskOfLossExpectationNeutral seller buyer damage probDamage = riskOfLossParameterizedExpectation seller buyer damage probDamage NeutralRisk
riskOfLossExpectationSeller seller buyer damage probDamage = riskOfLossParameterizedExpectation seller buyer damage probDamage SellerRisk
