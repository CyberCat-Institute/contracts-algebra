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

data Reinvest = Reinvest | DontReinvest deriving (Show, Eq, Ord)

-- pricedRound :: HowMuchToRaise -> z -> SeriesInvestment -> SeriesValuation -> Reinvest

equityFinancing name undefinedDecisionSpace =
  [opengame|
        inputs    : howMuchToRaise,valuation,performanceAtT1 ;
        feedback  : ;

        :----------------------------:

        inputs    : howMuchToRaise,valuation,performanceAtT1;
        feedback  : ;
        operation : dependentDecision name (const undefinedDecisionSpace);
        // TODO replace by adequate game among investors and other players
        outputs   : (investor,seriesInvestment), seriesValuation;
        returns   : exitPayoff;

        :----------------------------:

        outputs   : (investor,seriesInvestment), seriesValuation;
        returns   : exitPayoff;
    |]
