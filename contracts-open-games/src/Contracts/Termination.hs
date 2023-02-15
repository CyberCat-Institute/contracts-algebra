{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.Termination where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor

import Data.Tuple.Extra (uncurry3)

-- Termination of contracts

-------------------------------------
-- 0 Data types + auxiliary functions
-------------------------------------

data TerminateContract = Terminate | Continue
  deriving (Show, Eq, Ord)

terminateContract :: TerminateContract -> TerminateContract -> TerminateContract
terminateContract Terminate _         = Terminate
terminateContract _         Terminate = Terminate
terminateContract _         _         = Continue


--------------------
-- 1 Termination clauses
--------------------

-- | Determine the costs of a possible termination
terminationCosts costsOfTerminationFunction = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    inputs    :  ;
    feedback  : ;
    operation : forwardFunction $ costsOfTerminationFunction ;
    outputs   : costsSeller,costsBuyer ;
    returns   :  ;

    :-----:

    outputs   : costsSeller,costsBuyer ;
    returns   : ;
|]




-------------------------
-- 2 Game for Termination
-------------------------

-- | Seller decision to terminate contract
terminationClauseSeller seller buyer favorabilityCondition= [opengame|

    inputs    : terminated, breachOfContractBuyer, daysLimit ;
    feedback  : ;

    :-----:

    inputs    : terminated, breachOfContractBuyer, daysLimit;
    feedback  : ;
    operation : forwardFunction $ uncurry3 favorabilityCondition ;
    outputs   : canTerminate ;
    returns   : ;


    inputs    : canTerminate;
    feedback  : ;
    operation : dependentDecision seller (const [Terminate,Continue]) ;
    outputs   : terminateSeller ;
    returns   : costsTerminationSeller;


    :-----:

    outputs   : terminateSeller ;
    returns   : costsTerminationSeller;
|]


