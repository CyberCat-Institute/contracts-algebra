module contracts-open-games.src.Contracts.Safe.Exit where

import Contracts.Safe.Types
import Engine.Engine
import Preprocessor.Preprocessor

-- exit or no?
data ExitType = IPO | Merger | Dissolution deriving (Show, Eq, Ord)

-- | Payoff matrix for player i given i's action and j's action
exitMatrix :: ExitType -> Double -> Double
exitMatrix IPO x = (^) x 2 -- x^2 for exponential gains as the public company grows more investors and returns over time
exitMatrix Merger x = ((**) (x * 0.3) 2) + 60000 -- decaying reward over time due to the instant nature of a merger
exitMatrix Dissolution x = 0 * x -- Everyone loses in a dissolution

exitDecision ExitType valuation=
  [opengame|
        inputs    : CapTable;
        feedback  : ;

        :----------------------------:

        inputs    : CapTable;
        feedback  : ;
        operation : forwardFunction exitMatrix ExitType valuation;
        outputs   : [StockValPair] ;
        returns   : ;

        :----------------------------:

        outputs   : [StockValPair];
        returns   : ;
    |]