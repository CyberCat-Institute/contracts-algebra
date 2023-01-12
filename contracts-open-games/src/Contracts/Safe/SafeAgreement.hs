-- Imports here
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

------------------------------------------------------------------------------
module Contracts.Safe where

import Engine.Engine
import Preprocessor.Preprocessor

---------------
-- 0 Data types
-- Players: Investor, Company
-- Actions: cap is set to a certain percentage, then the investor and company can choose to settle or not settle
---------------
data SafeMove = Settle | DontSettle deriving (Eq, Ord, Show)

y :: Stochastic Double
y = distFromList [(0.0, 0.5), (100000.0, 0.2), (1000000.0, 0.2), (10000000.0, 0.008), (100000000.0, 0.0019999), (1000000000.0, 0.0000001)]

------------
-- 1 Payoffs
-- The payoff is the reward the investor gets depending on the valuation
------------

-- | Payoff matrix for player i given i's action and j's action
safeAgreementMatrix :: SafeMove -> SafeMove -> Double -> Double
-- safeAgreementMatrix company investor payoff = payoff
safeAgreementMatrix Settle Settle x = (^) x 2 -- x^2 for exponential gains as the valuation grows
safeAgreementMatrix Settle DontSettle x = ((**) (x * 0.3) 2) + 60000 -- decaying reward over time; 0.3 is the decay rate, 60000 is for payoff buffer
safeAgreementMatrix DontSettle Settle x = 0 * x -- neither parties gain if the company doesn't settle
safeAgreementMatrix DontSettle DontSettle x = ((**) (x * 0.3) 2) + 60000 -- Same outcome if investor doesn't settle

--------------------
-- safeAgreementMatrix _ _ = undefined

-- 2 Representation

-- | x is an exogenous variable
safeAgreement x =
  [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:

   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "Company" (const [Settle,DontSettle]);
   outputs   : decisionPlayer1 ;
   returns   : safeAgreementMatrix decisionPlayer1 decisionPlayer2 x;

   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "Investor" (const [Settle,DontSettle]);
   outputs   : decisionPlayer2 ;
   returns   : safeAgreementMatrix decisionPlayer2 decisionPlayer1 x;

   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]

-- Safe agreement 2: No x variable for valuation; endogenous valuation
safeAgreement2 =
  [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : nature y;
   outputs   : x;
   returns   : ;

   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "Company" (const [Settle,DontSettle]);
   outputs   : decisionPlayer1 ;
   returns   : safeAgreementMatrix decisionPlayer1 decisionPlayer2 x;

   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "Investor" (const [Settle,DontSettle]);
   outputs   : decisionPlayer2 ;
   returns   : safeAgreementMatrix decisionPlayer2 decisionPlayer1 x;

   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]

--------------------------
-- 3 Equilibrium analysis

isEquilibriumSafeAgreement strat x = generateIsEq $ evaluate (safeAgreement x) strat void

-- | Define pure single player strategies
cooperateStrategy :: Kleisli Stochastic () SafeMove
cooperateStrategy = pureAction Settle
-- ^ play _Cooperate_ with certainty

defectStrategy :: Kleisli Stochastic () SafeMove
defectStrategy = pureAction DontSettle
-- ^ play _Defect_ with certainty

-- | Combine single player's strategies into a tuple
strategTupleCooperate = cooperateStrategy ::- cooperateStrategy ::- Nil
-- ^ Both players cooperate with certainty

strategTupleDefect = defectStrategy ::- defectStrategy ::- Nil
-- ^ Both players defect with certainty

-- Show Diagnostic info of the game
showStats x = generateOutput $ evaluate (safeAgreement x) strategTupleCooperate void

showStatsEndogenous = generateOutput $ evaluate safeAgreement2 strategTupleCooperate void

-- isEquilibriumSafeAgreement strategTupleCooperate -- NOT an eq
-- isEquilibriumSafeAgreement strategTupleDefect -- eq