{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}


module Contracts.Safe.ExitDecision where

import Contracts.Safe.Types
import OpenGames.Engine.Engine
import OpenGames.Preprocessor

-- exit or no?
data ExitDecisionMatrix = Exit | Stay deriving (Show, Eq, Ord)

-- function to determine if the SAFE Investor will be backing out of the deal
safeExitDecision :: SeriesInvestment -> SeriesValuation -> CapTable -> CashOut -> CapTable -> ExitDecisionMatrix
safeExitDecision = undefined
-- FIXME Is this supposed to be a function or a decision?
-- FIXME This whole module does not make sense
-- FIXME Is this function supposed to compute the decision; if so why are there then several outputs below?
{-
exitDecision = [opengame|

        inputs    : investment,valuation,capTable ;
        feedback  : ;

        :----------------------------:

        inputs    : investment,valuation,capTable;
        feedback  : ;
        operation : dependentDecision "investor" (const [Exit,Stay]);
        outputs   : capTableNew, cashOut ;
        returns   : 0 ;
        // TODO check payoffs

        :----------------------------:

        outputs   : capTableNew, cashOut;
        returns   : ;
    |]
--}
