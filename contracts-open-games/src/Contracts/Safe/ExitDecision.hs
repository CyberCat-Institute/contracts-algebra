module Contracts.Safe.ExitDecision where

import Contracts.Safe.Types
import Engine.Engine
import Preprocessor.Preprocessor

-- exit or no?
data ExitDecisionMatrix = Exit | Stay deriving (Show, Eq, Ord)

-- function to determine if the SAFE Investor will be backing out of the deal
safeExitDecision :: SeriesInvestment -> SeriesValuation -> CapTable -> CashOut -> CapTable -> ExitDecisionMatrix

exitDecision =
  [opengame|
        inputs    : investment,valuation,capTable;
        feedback  : ;

        :----------------------------:

        inputs    : investment,valuation,capTable;
        feedback  : ;
        operation : dependentDecision safeExitDecision (const [Exit,Stay]);
        outputs   : ExitDecisionMatrix, CapTable, CashOut ;
        returns   : ;

        :----------------------------:

        outputs   : CapTable, CashOut;
        returns   : ;
    |]