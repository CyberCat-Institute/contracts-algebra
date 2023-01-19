module contracts-open-games.src.Contracts.Safe.Exit where

import Contracts.Safe.Types
import Engine.Engine
import Preprocessor.Preprocessor

-- exit or no?
data ExitType = IPO | Merger | Dissolution deriving (Show, Eq, Ord)

exitDecision ExitType =
  [opengame|
        inputs    : CapTable;
        feedback  : ;

        :----------------------------:

        inputs    : CapTable;
        feedback  : ;
        operation : // forward function?
        outputs   : [StockValPair] ;
        returns   : ;

        :----------------------------:

        outputs   : [StockValPair];
        returns   : ;
    |]