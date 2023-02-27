{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.Insurance.Clauses where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor

import Contracts.Types
-- Insurance arrangements


---------------
-- 0 Data types
---------------

data Insuree = Seller | Buyer | Both
  deriving (Show,Eq,Ord)

type InsuranceDate = Int

favorability :: (Insuree, SellerCosts, BuyerCosts, InsuranceDate, CurrentDate) -> (SellerCosts,BuyerCosts)
favorability (insuree,costsSeller,costsBuyer,deadline,time)
 | insuree == Seller && time < deadline = (costsSeller,0)
 | insuree == Buyer && time < deadline = (0,costsBuyer)
 | insuree == Both && time < deadline = (costsSeller,costsBuyer)
 | otherwise = (0,0)

--------------------
-- 1 Insurance clauses
--------------------

-- | Insurance in place for a period of time
insuranceClause seller buyer insuranceCostFunction = [opengame|

    inputs    : insuranceCondition, amountSeller,amountBuyer, deadline, time;
    feedback  : ;

    :-----:

    inputs    : insuranceCondition, amountSeller,amountBuyer, deadline, time ;
    feedback  : ;
    operation : forwardFunction $ insuranceCostFunction;
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


-- | Insurance in place for a period of time
insuranceClauseExogenous seller buyer insuranceCostFunction insuranceCondition  amountSeller amountBuyer  deadline  time = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    inputs    : insuranceCondition, amountSeller,amountBuyer, deadline, time ;
    feedback  : ;
    operation : forwardFunction $ insuranceCostFunction;
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

-----------------------
-- 2 Concrete Instances
-----------------------

-- Concrete insurance clauses (1: most favorable buyer; 3: most favorable seller)
insurance1 amountSeller amountBuyer deadline time = insuranceClauseExogenous "seller" "buyer" favorability Buyer amountSeller amountBuyer deadline time
insurance2 amountSeller amountBuyer deadline time = insuranceClauseExogenous "seller" "buyer" favorability Both amountSeller amountBuyer deadline time
insurance3 amountSeller amountBuyer deadline time = insuranceClauseExogenous "seller" "buyer" favorability Seller amountSeller amountBuyer deadline time
