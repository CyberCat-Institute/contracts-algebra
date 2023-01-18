{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}



module Contracts.Safe.NewMoney where


import Contracts.Safe.Types
import OpenGames.Engine.Engine
import OpenGames.Preprocessor

data RaiseDecision = Raise | DontRaise
  deriving (Show,Eq,Ord)

-- function to determine how much money to raise
-- newMoneyDecision :: SafeInvestment -> z -> HowMuchToRaise
-- newMoneyDecision investment z  = undefined




newMoney name distribution payoffFunction = [opengame|
  inputs    : investment, performanceAtT1;
  feedback  : ;

  :----------------------------:

  inputs    : investment, performanceAtT1 ;
  feedback  : ;
  operation : dependentDecision name (const [Raise,DontRaise]);
  outputs   : howMuchToRaise ;
  returns   : payoffFunction newInvestment companyShares exitPayoff;

  :----------------------------:

  outputs   : howMuchToRaise;
  returns   : newInvestment, companyShares, exitPayoff ;
    |]
