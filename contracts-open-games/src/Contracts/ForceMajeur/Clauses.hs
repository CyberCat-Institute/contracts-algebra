{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.ForceMajeur.Clauses where

import Contracts.Types
import OpenGames.Engine.Engine
import OpenGames.Preprocessor

-- Force Majeure Events
-- TODO Add conditionality for days after event happend

-------------------------------------
-- 0 Data types + auxiliary functions
-------------------------------------

data ForceMajeurLiability = Buyer | Both
  deriving (Show,Eq,Ord)

type ForceMajeurDate = Int

terminateContract :: TerminateContract -> TerminateContract -> TerminateContract
terminateContract Terminate _         = Terminate
terminateContract _         Terminate = Terminate
terminateContract _         _         = Continue


actionSpaceSeller :: (Bool,ForceMajeurLiability,ForceMajeurDate, CurrentDate) -> [TerminateContract]
actionSpaceSeller (eventHappened,condition,deadline,date)
  | eventHappened && condition == Both && deadline < date = [Continue,Terminate]
  | eventHappened && condition == Buyer && deadline < date = [Continue,Terminate]
  | otherwise = [Continue]

actionSpaceBuyer :: (Bool,ForceMajeurLiability,ForceMajeurDate, CurrentDate) -> [TerminateContract]
actionSpaceBuyer (eventHappened,condition,deadline,date)
  | eventHappened && condition == Both && deadline < date = [Continue,Terminate]
  | otherwise = [Continue]


--------------------
-- 1 Force majeur clause
--------------------

-- | Force majeur clause
forceMajeurClause seller buyer = [opengame|

    inputs    : eventHappened, forceMajeurCondition, deadline, time;
    feedback  : ;

    :-----:

    inputs    : eventHappened, forceMajeurCondition, deadline, time ;
    feedback  : ;
    operation : dependentDecision seller actionSpaceSeller ;
    outputs   : decisionSeller ;
    returns   : costTerminationSeller ;


    inputs    : eventHappened, forceMajeurCondition, deadline, time;
    feedback  : ;
    operation : dependentDecision buyer actionSpaceBuyer ;
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


-- | Force majeur clause exogenous inputs
forceMajeurClauseExogenous seller buyer eventHappened forceMajeurCondition  deadline  time = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    inputs    : eventHappened, forceMajeurCondition, deadline, time ;
    feedback  : ;
    operation : dependentDecision seller actionSpaceSeller ;
    outputs   : decisionSeller ;
    returns   : costTerminationSeller ;


    inputs    : eventHappened, forceMajeurCondition, deadline, time;
    feedback  : ;
    operation : dependentDecision buyer actionSpaceBuyer ;
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


  
-----------------------
-- 2 Concrete instances
-----------------------

-- Instantiate the different cases (1: neutral; 2: favoring seller)
forceMajeur1 eventHappened deadline  time = forceMajeurClauseExogenous "seller" "buyer" eventHappened Both  deadline time
forceMajeur2 eventHappened deadline  time = forceMajeurClauseExogenous "seller" "buyer" eventHappened Both  deadline time
