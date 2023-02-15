{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.ForceMajeur where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor

-- Force Majeure Events
-- TODO Add conditionality for days after event happend

-------------------------------------
-- 0 Data types + auxiliary functions
-------------------------------------

data ForceMajeurLiability = Buyer | Both
  deriving (Show,Eq,Ord)

data TerminateContract = Terminate | Continue
  deriving (Show, Eq, Ord)

terminateContract :: TerminateContract -> TerminateContract -> TerminateContract
terminateContract Terminate _         = Terminate
terminateContract _         Terminate = Terminate
terminateContract _         _         = Continue


--------------------
-- 1 Force majeur clause
--------------------

-- | Force majeur costs
-- Did the event happen? What are the associated costs then?
forceMajeurClause seller buyer forceMajeurCostFunction = [opengame|

    inputs    : eventHappened ;
    feedback  : ;

    :-----:

    inputs    : eventHappened ;
    feedback  : ;
    operation : forwardFunction $ forceMajeurCostFunction;
    outputs   : costsSeller,costsBuyer ;
    returns   : ;


    inputs    : costsSeller;
    feedback  : ;
    operation : addPayoffs seller ;
    outputs   :  ;
    returns   : ;


    inputs    : costsBuyer ;
    feedback  : ;
    operation : addPayoffs buyer ;
    outputs   :  ;
    returns   :  ;

    :-----:

    outputs   : ;
    returns   : ;
|]

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



-- | Game for the decision to terminate contract
terminateContractAfterForceMajeur seller buyer = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    inputs    : ;
    feedback  : ;
    operation : dependentDecision seller (const [Terminate,Continue]) ;
    outputs   : decisionSeller ;
    returns   : costTerminationSeller ;


    inputs    : ;
    feedback  : ;
    operation : dependentDecision buyer (const [Terminate,Continue]) ;
    outputs   : decisionBuyer ;
    returns   : costTerminationBuyer ;


    inputs    : decisionSeller,decisionBuyer ;
    feedback  : ;
    operation : forwardFunction $ uncurry terminateContract ;
    outputs   : terminateContract ;
    returns   :  ;

    :-----:

    outputs   : terminateContract ;
    returns   : costTerminationSeller,costTerminationBuyer ;
|]

