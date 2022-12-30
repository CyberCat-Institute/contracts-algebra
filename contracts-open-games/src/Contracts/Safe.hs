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

------------
-- 1 Payoffs
-- The payoff is the amount of cap table space that the
------------

-- | Payoff matrix for player i given i's action and j's action
safeAgreementMatrix :: SafeMove -> SafeMove -> Double -> Double
-- safeAgreementMatrix Company Investor = Payoff
-- TODO: add another variable for valuation cap

safeAgreementMatrix Settle Settle x = 2 * x
safeAgreementMatrix Settle DontSettle x = 0 * x
safeAgreementMatrix DontSettle Settle x = 0 * x
safeAgreementMatrix DontSettle DontSettle x = -1 * x

--------------------
-- 2 Representation

-- | Prisoner's dilemma in verbose form; x is an exogenous variable
safeAgreement x =
  [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "player1" (const [Settle,DontSettle]);
   outputs   : decisionPlayer1 ;
   returns   : safeAgreementMatrix decisionPlayer1 decisionPlayer2 x;

   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "player2" (const [Settle,DontSettle]);
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

-- isEquilibriumSafeAgreement strategTupleCooperate -- NOT an eq
-- isEquilibriumSafeAgreement strategTupleDefect -- eq
