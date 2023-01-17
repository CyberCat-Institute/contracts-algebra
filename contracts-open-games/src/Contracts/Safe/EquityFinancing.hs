module Contracts.Safe.EquityFinancing where

import Contracts.Safe.Types
import Engine.Engine
import Preprocessor.Preprocessor

data Reinvest = Reinvest | DontReinvest deriving (Show, Eq, Ord)

pricedRound :: HowMuchToRaise -> z -> SeriesInvestment -> SeriesValuation -> Reinvest

equityFinancing =
  [opengame|
        inputs    : howMuchToRaise,z;
        feedback  : ;

        :----------------------------:

        inputs    : howMuchToRaise,z;
        feedback  : ;
        operation : dependentDecision pricedRound (const [Reinvest,DontReinvest]);
        outputs   : SeriesInvestment, SeriesValuation;
        returns   : CashOut;

        :----------------------------:

        outputs   : SeriesInvestment, SeriesValuation;
        returns   : CashOut;
    |]