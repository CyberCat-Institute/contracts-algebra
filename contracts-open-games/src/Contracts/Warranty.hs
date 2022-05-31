{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.Warranty where

import Engine.Engine
import Preprocessor.Preprocessor

-- Warranty rules
-- This module describes clauses where warranty costs are regulated


---------------
-- 0 Data types
---------------

data Warranty = AsIs | LimitedWarranty | FullWaranty
  deriving (Show,Eq,Ord)


---------------------
-- 1 Warranty clauses
---------------------
-- | Warranty costs associated with the contract
warrantyCosts seller buyer warrantyCostFunction= [opengame|

    inputs    : warranty ;
    feedback  : ;

    :-----:

    inputs    : warranty ;
    feedback  : ;
    operation : forwardFunction  warrantyCostFunction;
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
