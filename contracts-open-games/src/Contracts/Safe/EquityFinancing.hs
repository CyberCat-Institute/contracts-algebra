module Contracts.Safe.EquityFinancing where

import Contracts.Safe.Types
import Engine.Engine
import Preprocessor.Preprocessor

pricedRound :: HowMuchToRaise -> z -> SeriesInvestment -> SeriesValuation

equityFinancing =
  [opengame|
        inputs    : howMuchToRaise,z;
        feedback  : ;

        :----------------------------:

        inputs    : howMuchToRaise,z;
        feedback  : ;
        operation : pricedRound;
        outputs   : SeriesInvestment, SeriesValuation;
        returns   : CashOut;

        :----------------------------:

        outputs   : SeriesInvestment, SeriesValuation;
        returns   : CashOut;
    |]