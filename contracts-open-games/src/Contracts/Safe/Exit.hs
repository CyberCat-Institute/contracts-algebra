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
import qualified Data.Map.Strict  as M
import OpenGames.Engine.Engine
import OpenGames.Preprocessor

-- exit or no?
data ExitType = IPO | Merger | Dissolution deriving (Show, Eq, Ord)

-- | Determines the overall company evaluation
exitValue :: ExitType -> Double -> Double
exitValue IPO x =  x ** 2 -- x^2 for exponential gains as the public company grows more investors and returns over time
exitValue Merger x = (x * 0.3) ** 2 + 60000 -- decaying reward over time due to the instant nature of a merger
exitValue Dissolution _ = 0 -- Everyone loses in a dissolution
-- FIXME the exitmatrix function above has two inputs; below it is used with three inputs

-- | Computes the value for each stakeholder according to the cap-table
computeValueForStakeHolders :: CapTable -> CashOut -> CashOutMap
computeValueForStakeHolders capTable valuation =
  M.map (\share -> share * valuation) capTable

-- | Game which determines the value for each investor at exit
exitOutcome  =
  [opengame|
        inputs    : exitType, valuation, capTable;
        feedback  : ;

        :----------------------------:

        inputs    : exitType, valuation  ;
        feedback  : ;
        operation : forwardFunction $ uncurry exitValue ;
        outputs   : valueCompany ;
        returns   : ;
        // Produces the overall company value

        inputs    : capTable, valueCompany  ;
        feedback  : ;
        operation : forwardFunction $ uncurry computeValueForStakeHolders;
        outputs   : cashOutMap ;
        returns   : ;
        // Produces the value for each investor

        :----------------------------:

        outputs   : cashOutMap ;
        returns   : ;
    |]


-- TODO: There should be another interface where the value is determined 
