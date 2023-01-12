module Contracts.Safe.ExitDecision where

import Contracts.Safe.Types
import Engine.Engine
import Preprocessor.Preprocessor

-- function to determine if the SAFE Investor will be backing out of the deal
safeExitDecision :: SeriesInvestment -> SeriesValuation -> CapTable -> CashOut -> CapTable

exitDecision =
  [opengame|
        inputs    : investment,valuation,capTable;
        feedback  : ;

        :----------------------------:

        inputs    : investment,valuation,capTable;
        feedback  : ;
        operation : safeExitDecision;
        outputs   : CapTable, CashOut ;
        returns   : ;

        :----------------------------:

        outputs   : CapTable, CashOut;
        returns   : ;
    |]