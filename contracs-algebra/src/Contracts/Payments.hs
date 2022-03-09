{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.Payments where

--import Engine.Engine
--import Preprocessor.Preprocessor

-- Payment clauses


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
--------------------
-- 3 Building Blocks
--------------------

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


--------------------
-- 4 Strategies
--------------------

strategyLowDiligence, strategyHighDiligence :: List '[Kleisli Stochastic () Diligence, Kleisli Stochastic Double Integer]
strategyLowDiligence = pureAction Low ::- pureAction 1 ::-  Nil
strategyHighDiligence = pureAction High ::- pureAction 1 ::- Nil

eqCapPayment strat = generateIsEq $  evaluate (capPayment parameters) strat void

eqClientPayment strat = generateIsEq $  evaluate (clientPayment parameters) strat void


{--
-- If the provider pays for the problems, he internalizes the costs
-- Not an equilibrium given the stochastic nature of the process
eqCapPayment strategyLowDiligence

>>>>> Output
----Analytics begin----
 Strategies are NOT in equilibrium. Consider the following profitable deviations:

Player: provider
Optimal Move: High
Current Strategy: fromFreqs [(Low,1.0)]
Optimal Payoff: -6.800000000000001
Current Payoff: -7.0
Observable State: ()
Unobservable State: "(((),()),())"
 --other game--
 --No more information--
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:
----Analytics end---
<<<<<<<<<<<<<

-- Eq
eqCapPayment strategyHighDiligence

>>>> Output

----Analytics begin----
 Strategies are in equilibrium
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:
----Analytics end----

<<<<<<<<<<<

-- If the client pays for the expenses, the provider has no incentive act diligently

-- Not an eq.
eqClientPayment strategyHighDiligence

>>>>>> Output
----Analytics begin----
 Strategies are NOT in equilibrium. Consider the following profitable deviations:

Player: provider
Optimal Move: Low
Current Strategy: fromFreqs [(High,1.0)]
Optimal Payoff: 0.0
Current Payoff: -1.0
Observable State: ()
Unobservable State: "(((),()),())"
 --other game--
 --No more information--
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:
----Analytics end---
<<<<<<<<<<<<<<<

-- Eq.
eqClientPayment strategyLowDiligence
>>>>>> Output
----Analytics begin----
 Strategies are in equilibrium
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:
----Analytics end----

----Analytics end---
<<<<<<<<<<<<<<<
-}
