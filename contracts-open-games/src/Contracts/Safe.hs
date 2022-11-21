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
import Language.Haskell.TH (safe)
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
safeAgreementMatrix :: SafeMove -> SafeMove -> Double
-- safeAgreementMatrix Company Investor = Payoff
safeAgreementMatrix Settle Settle = 2
safeAgreementMatrix Settle DontSettle = -1
safeAgreementMatrix DontSettle Settle = 3
safeAgreementMatrix DontSettle DontSettle = 0

--------------------
-- 2 Representation

-- | Prisoner's dilemma in verbose form
safeAgreement =
  [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "player1" (const [Settle,DontSettle]);
   outputs   : decisionPlayer1 ;
   returns   : safeAgreementMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "player2" (const [Settle,DontSettle]);
   outputs   : decisionPlayer2 ;
   returns   : safeAgreementMatrix decisionPlayer2 decisionPlayer1 ;

   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]

--------------------------
-- 3 Equilibrium analysis

isEquilibriumSafeAgreement strat = generateIsEq $ evaluate safeAgreement strat void

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

isEquilibriumSafeAgreement strategTupleCooperate -- NOT an eq
isEquilibriumSafeAgreement strategTupleDefect -- eq