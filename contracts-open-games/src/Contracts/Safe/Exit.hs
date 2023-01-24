{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Contracts.Safe.Exit where

import Contracts.Safe.Types
import OpenGames.Engine.Engine
import OpenGames.Preprocessor

import qualified Data.Map.Strict as M

-- exit or no?
data ExitType = IPO | Merger | Dissolution deriving (Show, Eq, Ord)

-- | Payoff matrix for player i given i's action and j's action
exitMatrix :: ExitType -> Double -> Double
exitMatrix IPO x = (^) x 2 -- x^2 for exponential gains as the public company grows more investors and returns over time
exitMatrix Merger x = ((**) (x * 0.3) 2) + 60000 -- decaying reward over time due to the instant nature of a merger
exitMatrix Dissolution x = 0 * x -- Everyone loses in a dissolution
-- FIXME the exitmatrix function above has two inputs; below it is used with three inputs


computeValueForStakeHolders :: CapTable -> CashOut -> CashOutMap
computeValueForStakeHolders capTable valuation =
  M.map (\share -> share * valuation) capTable

exitDecision exitType valuation=
  [opengame|
        inputs    : valuation, capTable ;
        feedback  : ;

        :----------------------------:

        inputs    : valuation ;
        feedback  : ;
        operation : forwardFunction $ exitMatrix exitType;
        outputs   : valueCompany ;
        returns   : ;

        inputs    : capTable, valuation  ;
        feedback  : ;
        operation : forwardFunction $ uncurry computeValueForStakeHolders;
        outputs   : cashOutMap ;
        returns   : ;


        :----------------------------:

        outputs   : cashOutMap ;
        returns   : ;
    |]
