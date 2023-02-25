{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.Termination.Clauses where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor

import Data.Tuple.Extra (uncurry3)
import Data.Bool (Bool(True))

-- Termination of contracts

-------------------------------------
-- 0 Data types + auxiliary functions
-------------------------------------

data TerminateContract = Continue | Terminate
  deriving (Show, Eq, Ord)

data ContractBreach = ContractBreachYes | ContractBreachNo
  deriving (Show, Eq, Ord)

data BreachCurable = BreachCurableYes | BreachCurableNo
  deriving (Show, Eq, Ord)

data NonPayment = NonPaymentYes | NonPaymentNo
  deriving (Show,Eq, Ord)

data LegalIssues = LegalIssuesYes | LegalIssuesNo
  deriving (Show,Eq, Ord)

data Produced = ProducedYes | ProducedNotYet
  deriving (Show,Eq, Ord)

type TerminationDeadline = Int

type CurationDeadline = Int

type PaymentDeadline = Int

type CurrentDate = Int

terminateContract :: TerminateContract -> TerminateContract -> TerminateContract
terminateContract Terminate _         = Terminate
terminateContract _         Terminate = Terminate
terminateContract _         _         = Continue


favorability1 :: (TerminationDeadline, CurrentDate, ContractBreach, BreachCurable, CurationDeadline) ->  Bool
favorability1 (deadlineTermination,currentDate,breached,curable,deadlineCuration)
  | deadlineTermination < currentDate && breached == ContractBreachYes && curable == BreachCurableYes && deadlineCuration < currentDate = True
  | deadlineTermination < currentDate && breached == ContractBreachYes && curable == BreachCurableNo  = True
  | otherwise = False

favorability2 :: (TerminationDeadline, CurrentDate) ->  Bool
favorability2 (deadlineTermination,currentDate)
  | deadlineTermination < currentDate  = True
  | otherwise = False

favorability3 :: (TerminationDeadline, CurrentDate, ContractBreach, BreachCurable, CurationDeadline, NonPayment, PaymentDeadline, LegalIssues) ->  Bool
favorability3 (deadlineTermination,currentDate,breached,curable,deadlineCuration, nonPayment, paymentDeadline, legalIssues)
  | deadlineTermination < currentDate = True
  | breached == ContractBreachYes && curable == BreachCurableYes && deadlineCuration < currentDate = True
  | breached == ContractBreachYes && curable == BreachCurableNo = True
  | nonPayment == NonPaymentYes && paymentDeadline < currentDate = True
  | legalIssues == LegalIssuesYes = True
  | otherwise = False

favorability4 :: (TerminationDeadline, CurrentDate, ContractBreach, BreachCurable, CurationDeadline, TerminationDeadline, Produced) ->  Bool
favorability4 (deadlineTermination,currentDate,breached,curable,deadlineCuration, deadlineTerminationSpeedup, alreadyProduced)
  | deadlineTermination < currentDate && alreadyProduced == ProducedNotYet = True
  | deadlineTerminationSpeedup < currentDate  && breached == ContractBreachYes && curable == BreachCurableNo  = True
  | otherwise = False


actionSpace :: Bool -> [TerminateContract]
actionSpace True  = [Continue,Terminate]
actionSpace False = [Continue] 


------------------------
-- 1 Termination clauses
------------------------

-- | Decision to terminate contract favoring buyer
terminationClause1 name= [opengame|

    inputs    : deadlineTermination,currentDate,breached,curable,deadlineCuration ;
    feedback  : ;

    :-----:

    inputs    : deadlineTermination,currentDate,breached,curable,deadlineCuration;
    feedback  : ;
    operation : forwardFunction $ favorability1 ;
    outputs   : canTerminate ;
    returns   : ;


    inputs    : canTerminate;
    feedback  : ;
    operation : dependentDecision name actionSpace ;
    outputs   : terminateDecision ;
    returns   : costsTermination;


    :-----:

    outputs   : terminateDecision ;
    returns   : costsTermination;
|]


-- | Decision to terminate contract with exogenous parameters favoring buyer
terminationClauseExogenous1 name  deadlineTermination currentDate breached curable deadlineCuration = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    inputs    : deadlineTermination,currentDate,breached,curable,deadlineCuration;
    feedback  : ;
    operation : terminationClause1 name ;
    outputs   : terminateDecision ;
    returns   : costsTermination;

    :-----:

    outputs   : terminateDecision ;
    returns   : costsTermination;

|]

-- | Decision to terminate contract favoring buyer
terminationClause2 name= [opengame|

    inputs    : deadlineTermination,currentDate ;
    feedback  : ;

    :-----:

    inputs    : deadlineTermination,currentDate;
    feedback  : ;
    operation : forwardFunction $ favorability2 ;
    outputs   : canTerminate ;
    returns   : ;


    inputs    : canTerminate;
    feedback  : ;
    operation : dependentDecision name actionSpace ;
    outputs   : terminateDecision ;
    returns   : costsTermination;


    :-----:

    outputs   : terminateDecision ;
    returns   : costsTermination;
|]


-- | Decision to terminate contract with exogenous parameters favoring buyer
terminationClauseExogenous2 name  deadlineTermination currentDate = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    inputs    : deadlineTermination,currentDate;
    feedback  : ;
    operation : terminationClause2 name ;
    outputs   : terminateDecision ;
    returns   : costsTermination;

    :-----:

    outputs   : terminateDecision ;
    returns   : costsTermination;

|]

-- | Decision to terminate contract favoring buyer
terminationClause3 name= [opengame|

    inputs    : deadlineTermination,currentDate,breached,curable,deadlineCuration, nonPayment, paymentDeadline, legalIssues ;
    feedback  : ;

    :-----:

    inputs    : deadlineTermination,currentDate,breached,curable,deadlineCuration, nonPayment, paymentDeadline, legalIssues;
    feedback  : ;
    operation : forwardFunction $ favorability3 ;
    outputs   : canTerminate ;
    returns   : ;


    inputs    : canTerminate;
    feedback  : ;
    operation : dependentDecision name actionSpace ;
    outputs   : terminateDecision ;
    returns   : costsTermination;


    :-----:

    outputs   : terminateDecision ;
    returns   : costsTermination;
|]


-- | Decision to terminate contract with exogenous parameters favoring buyer
terminationClauseExogenous3 name deadlineTermination currentDate breached curable deadlineCuration nonPayment paymentDeadline legalIssues = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    inputs    : deadlineTermination,currentDate,breached,curable,deadlineCuration, nonPayment, paymentDeadline, legalIssues;
    feedback  : ;
    operation : terminationClause3 name ;
    outputs   : terminateDecision ;
    returns   : costsTermination;

    :-----:

    outputs   : terminateDecision ;
    returns   : costsTermination;

|]

-- | Decision to terminate contract favoring buyer
terminationClause4 name= [opengame|

    inputs    : deadlineTermination,currentDate,breached,curable,deadlineCuration, deadlineTerminationSpeedup, alreadyProduced ;
    feedback  : ;

    :-----:

    inputs    : deadlineTermination,currentDate,breached,curable,deadlineCuration, deadlineTerminationSpeedup, alreadyProduced ;
    feedback  : ;
    operation : forwardFunction $ favorability4 ;
    outputs   : canTerminate ;
    returns   : ;


    inputs    : canTerminate;
    feedback  : ;
    operation : dependentDecision name actionSpace ;
    outputs   : terminateDecision ;
    returns   : costsTermination;


    :-----:

    outputs   : terminateDecision ;
    returns   : costsTermination;
|]


-- | Decision to terminate contract with exogenous parameters favoring buyer
terminationClauseExogenous4 name deadlineTermination currentDate breached curable deadlineCuration deadlineTerminationSpeedup alreadyProduced= [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    inputs    : deadlineTermination,currentDate,breached,curable,deadlineCuration, deadlineTerminationSpeedup, alreadyProduced;
    feedback  : ;
    operation : terminationClause4 name ;
    outputs   : terminateDecision ;
    returns   : costsTermination;

    :-----:

    outputs   : terminateDecision ;
    returns   : costsTermination;

|]




-----------------------
-- 2 Concrete Instances
-----------------------

-- Instantiate the different cases for seller (1 weakest; 4 strongest)
terminationSeller1 currentDate breached curable = terminationClauseExogenous1 "seller" 30 currentDate breached curable 10 
terminationSeller2 currentDate breached curable = terminationClauseExogenous1 "seller" 15 currentDate breached curable 10 
terminationSeller3 currentDate = terminationClauseExogenous2 "seller"  30 currentDate
terminationSeller4 currentDate breached curable nonPayment legalIssues = terminationClauseExogenous3 "seller" 30 currentDate breached curable 10 nonPayment 10 legalIssues

-- Instantiate the different cases for buyer (1 weakest; 3 strongest)
terminateBuyer1 currentDate breached curable = terminationClauseExogenous1 "buyer" 30 currentDate breached curable 10
terminateBuyer2 currentDate breached curable alreadyProduced = terminationClauseExogenous4 "buyer" 30 currentDate breached curable 10 10 alreadyProduced
terminateBuyer3 currentDate = terminationClauseExogenous2 "buyer" 30 currentDate 
