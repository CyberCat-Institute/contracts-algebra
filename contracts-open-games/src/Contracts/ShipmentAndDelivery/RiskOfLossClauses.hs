{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.ShipmentAndDelivery.RiskOfLossClauses where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor

import Contracts.ShipmentAndDelivery.Types
import Contracts.Types

-- This module describes clauses which touch on risk of loss

------------------------
-- 0 Auxiliary Functions
------------------------


-- | Given a regime how is held responsible for a damage, how is the damage distributed?
riskOfLossFunction :: Player ->  Costs ->  (SellerCosts,BuyerCosts)
riskOfLossFunction player  costs
  | player == Buyer  = (0,-costs)
  | player == Seller = (-costs,0)

-- | How is the mode of risk determined?
-- Depends on the delivery conditions and where the damage occurs
determineWhoLoses :: ModeOfRiskOfLoss -> Player -> BuyerReceived ->  Player
determineWhoLoses DeliveryLocation  Buyer  True  = Buyer
determineWhoLoses DeliveryLocation  Buyer  False = Seller
determineWhoLoses DeliveryLocation  Seller _     = Seller
determineWhoLoses HandOverToCarrier Seller _     = Seller
determineWhoLoses HandOverToCarrier Buyer  _     = Buyer

-- | What is the probability distribution of damage?
probabilityOfLoss :: Costs -> Stochastic (Costs, Player, BuyerReceived)
probabilityOfLoss costs =
  uniformDist [(costs,Buyer,True),(costs,Buyer,False),(costs,Seller,True),(costs,Seller,False)
              ,(0,Buyer,True),(0,Buyer,False),(0,Seller,True),(0,Seller,False)]

----------------------
-- 1 RiskOfLossClauses
----------------------
-- | Generic risk of loss distribution
riskOfLoss seller buyer damageFunction mode = [opengame|

    inputs    : damage, locationDamage, buyerReceived ;
    feedback  : ;

    :-----:

    inputs    : locationDamage, buyerReceived ;
    feedback  : ;
    operation : forwardFunction $ uncurry $ determineWhoLoses mode ;
    outputs   : whoLoses ;
    returns   : ;


    inputs    : whoLoses, damage  ;
    feedback  : ;
    operation : forwardFunction $ uncurry damageFunction;
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

    outputs   : costsSeller,costsBuyer ;
    returns   : ;
|]


-- | Damage is fed as an exogenous parameter
riskOfLossExogenous seller buyer damageFunction damage location buyerReceived mode= [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    inputs    : damage, location, buyerReceived ;
    feedback  : ;
    operation : riskOfLoss seller buyer damageFunction mode ;
    outputs   : costsSeller,costsBuyer;
    returns   : ;

    :-----:

    outputs   : costsSeller,costsBuyer;
    returns   : ;
|]


  
-- | Specialize to above cost function and fix probabilities
riskOfLossParameterized seller buyer damage location buyerReceived mode = riskOfLossExogenous seller buyer riskOfLossFunction damage location buyerReceived mode

riskOfLoss1 seller buyer damage = riskOfLossParameterized seller buyer damage Buyer True DeliveryLocation
riskOfLoss2 seller buyer damage = riskOfLossParameterized seller buyer damage Buyer True HandOverToCarrier
riskOfLoss3 seller buyer damage = riskOfLossParameterized seller buyer damage Buyer False DeliveryLocation
riskOfLoss4 seller buyer damage = riskOfLossParameterized seller buyer damage Buyer False HandOverToCarrier
riskOfLoss5 seller buyer damage = riskOfLossParameterized seller buyer damage Seller True DeliveryLocation
riskOfLoss6 seller buyer damage = riskOfLossParameterized seller buyer damage Seller True HandOverToCarrier
riskOfLoss7 seller buyer damage = riskOfLossParameterized seller buyer damage Seller False DeliveryLocation
riskOfLoss8 seller buyer damage = riskOfLossParameterized seller buyer damage Seller False HandOverToCarrier




----------------------------------
-- 2 RiskOfLoss Clauses -- ex ante
----------------------------------

-- | Generic risk of loss and distribution of costs
-- NOTE Not clear whether the acceptance decision should be also allocated randomly
riskOfLossExpectation seller buyer  damageFunction mode probabilityDistribution = [opengame|

    inputs    : costs ;
    feedback  : ;

    :-----:

    inputs    : costs ;
    feedback  : ;
    operation : liftStochasticForward $ probabilityDistribution;
    outputs   : damage, locationDamaged, buyerReceived;
    returns   : ;

    inputs    : damage, locationDamaged, buyerReceived ;
    feedback  : ;
    operation : riskOfLoss seller buyer damageFunction mode;
    outputs   : costsSeller,costsBuyer;
    returns   : ;

    :-----:

    outputs   : costsSeller,costsBuyer;
    returns   : ;
|]

riskOfLossExpectationExogenous seller buyer costs damageFunction mode probabilityDistribution = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    inputs    : costs ;
    feedback  : ;
    operation : riskOfLossExpectation seller buyer  damageFunction mode probabilityDistribution;
    outputs   : costsSeller,costsBuyer;
    returns   : ;


    :-----:

    outputs   : costsSeller,costsBuyer;
    returns   : ;
|]


  
-- | Specialize to above cost function and fix probabilities
riskOfLossParameterizedExpectationExogenous seller buyer costs mode = riskOfLossExpectationExogenous seller buyer costs riskOfLossFunction mode probabilityOfLoss

riskOfLossExpectationBuyer seller buyer damage probDamage = riskOfLossParameterizedExpectationExogenous seller buyer damage  DeliveryLocation
riskOfLossExpectationSeller seller buyer damage probDamage = riskOfLossParameterizedExpectationExogenous seller buyer damage HandOverToCarrier

