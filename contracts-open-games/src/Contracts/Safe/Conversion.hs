module Contracts.Safe.Conversion where

import Contracts.Safe.Types
import Engine.Engine
import Preprocessor.Preprocessor

equityConversion :: SafeInvestment -> SeriesValuation -> StockValPair -> CapTable

conversion =
  [opengame|
        inputs    : investment,valuation;
        feedback  : ;

        :----------------------------:

        inputs    : investment,valuation;
        feedback  : ;
        operation : equityConversion;
        outputs   : StockValPair, CapTable ;
        returns   : ;

        :----------------------------:

        outputs   : StockValPair, CapTable;
        returns   : ;
    |]