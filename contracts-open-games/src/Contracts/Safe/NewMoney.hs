module Contracts.Safe.NewMoney where

import Contracts.Safe.Types
import Engine.Engine
import Preprocessor.Preprocessor

-- function to determine how much money to raise
newMoneyDecision :: SafeInvestment -> z -> HowMuchToRaise

newMoney =
  [opengame|
        inputs    : investment,z ;
        feedback  : ;

        :----------------------------:

        inputs    : investment,z ;
        feedback  : ;
        operation : newMoneyDecision;
        outputs   : HowMuchToRaise ;
        returns   : ;

        :----------------------------:

        outputs   : HowMuchToRaise;
        returns   : ;
    |]