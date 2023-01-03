{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.Payments.Payments where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor

-- Payment clauses
-- This module describes a game where expenses are charged by different parties
-- It also describes a simple module which determines punishment in case of late payments


---------------
-- 0 Data types
---------------
-- Diligence level by company
data Diligence = High | Low
  deriving (Ord,Eq,Show)

data Parameter = Parameter
   { costs :: Double
   , price :: Double
   , utility :: Double
   , p :: Double
   , delta :: Double
   , lowExpenses :: Double
   , highExpenses :: Double
   }

type Price = Double

type Days  = Int

type Interest = Double

parameters = Parameter 1 8 10 0.7 0.2 4 10



------------
-- 1 Payoffs
------------

-- Being diligent is costly
diligenceCosts High c = - c
diligenceCosts _    _ = 0


------------------------
-- 2 Auxiliary functions
------------------------

-- expenses depend on Diligence
-- FIXME note the boundaries of the probability
expensesProb p delta Low = p - delta
expensesProb p _     _   = p

-- distribution of expenses depends on probability of low expenses (affected by diligence)
expensesDistribution lowExpenses highExpenses p = distFromList [(lowExpenses,p),(highExpenses, (1-p))]

-- non decision
nonDecision = [1]

-- Payment interest
paymentLate :: Interest -> Price -> Days -> Price
paymentLate interest price delayedDays =
  price*interest* (fromIntegral delayedDays) 



--------------------
-- 3 Payment clauses
--------------------
-- | Payment module in case of late payments
paymentSettlement seller buyer interestRate = [opengame|

    inputs    : price, daysLate ;
    feedback  : ;

    :-----:

    inputs    : price, daysLate ;
    feedback  : ;
    operation : forwardFunction $ uncurry $ paymentLate interestRate;
    outputs   : penalty ;
    returns   : ;


    inputs    : penalty;
    feedback  : ;
    operation : addPayoffs seller ;
    outputs   :  ;
    returns   : ;


    inputs    : -penalty ;
    feedback  : ;
    operation : addPayoffs buyer ;
    outputs   :  ;
    returns   :  ;

    :-----:

    outputs   : ;
    returns   : ;
|]

-- | Payment module in case of late payments
paymentSettlementEndogenousRoles  interestRate = [opengame|

    inputs    : price, daysLate, seller, buyer ;
    feedback  : ;

    :-----:

    inputs    : price, daysLate ;
    feedback  : ;
    operation : forwardFunction $ uncurry $ paymentLate interestRate;
    outputs   : penalty ;
    returns   : ;


    inputs    : seller,penalty;
    feedback  : ;
    operation : addRolePayoffs ;
    outputs   :  ;
    returns   : ;


    inputs    : buyer, -penalty ;
    feedback  : ;
    operation : addRolePayoffs ;
    outputs   :  ;
    returns   :  ;

    :-----:

    outputs   : ;
    returns   : ;
|]


-------------------
-- 4 Expense models
-------------------
-- This module describes a specific use case where the amount of expenses are affects by actions of the players involved

-- | Expenses are generated depending on exogenous parameters and the level of diligence exercised
expenseFunction p delta lowExpenses highExpenses = [opengame|

    inputs    : diligence ;
    feedback  : ;

    :-----:

    inputs    : diligence ;
    feedback  : ;
    operation : forwardFunction (expensesProb p delta) ;
    outputs   : prob ;
    returns   : ;

    inputs    : prob ;
    feedback  : ;
    operation : liftStochasticForward $ expensesDistribution lowExpenses highExpenses;
    outputs   : expenses ;
    returns   : ;

    :-----:

    outputs   : expenses ;
    returns   : ;
|]

-- | Provider chooses diligence levels; higher diligence causes costs
providerAction Parameter{..} = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    inputs    : ;
    feedback  : ;
    operation : dependentDecision "provider" (const [High,Low]) ;
    outputs   : diligence ;
    returns   : diligenceCosts diligence costs;


    inputs    : (utility - price) ;
    feedback  : ;
    operation : addPayoffs "client" ;
    outputs   : ;
    returns   : ;

    inputs    : diligence ;
    feedback  : ;
    operation : expenseFunction p delta lowExpenses highExpenses ;
    outputs   : expenses ;
    returns   : ;

    :-----:

    outputs   : expenses ;
    returns   : ;
|]

-- | Scenario 1:  Provider pays for expenses
capPayment par = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    inputs    : ;
    feedback  : ;
    operation : providerAction par ;
    outputs   : expenses ;
    returns   : ;


    inputs    : expenses ;
    feedback  : ;
    operation : dependentDecision "provider" (const nonDecision) ;
    outputs   : notUsed ;
    returns   : -expenses ;

    :-----:

    outputs   :  ;
    returns   : ;
|]

-- | Scenario 2: Client pays for expenses that occur
clientPayment par = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    inputs    : ;
    feedback  : ;
    operation : providerAction par ;
    outputs   : expenses ;
    returns   : ;


    inputs    : expenses ;
    feedback  : ;
    operation : dependentDecision "client" (const nonDecision) ;
    outputs   : notUsed ;
    returns   : -expenses ;

    :-----:

    outputs   : ;
    returns   : ;
|]



