{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Contracts.Safe.EquityFinancing where

import Contracts.Safe.Types
import OpenGames.Engine.Engine
import OpenGames.Preprocessor

data SAFEReinvest = Reinvest | DontReinvest deriving (Show, Eq, Ord)

-- pricedRound :: HowMuchToRaise -> z -> SeriesInvestment -> SeriesValuation -> Reinvest

equityFinancing name undefinedDecisionSpace =
  [opengame|
        inputs    : howMuchToRaise,valuation,performanceAtT1 ;
        feedback  : ;

        :----------------------------:
        inputs    :      ;
        feedback  :      ;
        operation : dependentDecision name (const [Reinvest, DontReinvest]);;
        outputs   : SAFEReinvest;
        returns   : ;
   
        inputs    : howMuchToRaise,valuation,performanceAtT1,Reinvest;
        feedback  : ;
        operation : dependentDecision name (const undefinedDecisionSpace);
        // TODO replace by adequate game among investors and other players
        outputs   : CapTable, SeriesValuation;
        returns   : exitPayoff;

        :----------------------------:

        outputs   : CapTable, SeriesValuation;
        returns   : exitPayoff;
    |]
