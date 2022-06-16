{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.Insurance where

import Engine.Engine
import Preprocessor.Preprocessor

-- Insurance arrangements


---------------
-- 0 Data types
---------------

data Insuree = Seller | Buyer | Both
  deriving (Show,Eq,Ord)


--------------------
-- 1 Insurance clauses
--------------------

-- | Insurance 
insuranceClause seller buyer insuranceCostFunction = [opengame|

    inputs    : amount, periodOfTime ;
    feedback  : ;

    :-----:

    inputs    : amount, periodOfTime ;
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


