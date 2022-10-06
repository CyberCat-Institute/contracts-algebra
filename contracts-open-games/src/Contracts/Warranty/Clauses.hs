{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.Warranty.Clauses where

import Contracts.Warranty.Types
import Contracts.Types
import Engine.Engine
import Preprocessor.Preprocessor

-- Warranty rules
-- This module describes clauses where warranty costs are regulated


---------------
-- 0 Data types
---------------


-- | Warranty cost function that makes an assumption how damages are to be distributed
warrantyCostFunction ::  ShareOfWarranty ->  Warranty -> Costs -> (Costs,Costs)
warrantyCostFunction  _ NoWarranty  costs = (0,-costs)
warrantyCostFunction  share LimitedWarranty costs = (-(share*costs),-((1-share)*costs))
warrantyCostFunction  _ FullWarranty costs = (-costs,0)

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
    operation : forwardFunction warrantyCostFunction;
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

-- | Exogenous warranty costs
warrantyCostsExogenous seller buyer costs warrantyCostFunction = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:

    inputs    : costs ;
    feedback  : ;
    operation : warrantyCosts seller buyer warrantyCostFunction ;
    outputs   : ;
    returns   : ;

    :-----:

    outputs   : ;
    returns   : ;
|]



-- | Specialize to different warranty situations and cost function as above
warrantyCostsNoWarranty seller buyer costs shareOfWarranty  = warrantyCostsExogenous seller buyer costs (warrantyCostFunction shareOfWarranty NoWarranty)
warrantyCostsLimitedWarranty seller buyer costs shareOfWarranty  = warrantyCostsExogenous seller buyer costs (warrantyCostFunction shareOfWarranty LimitedWarranty)
warrantyCostsFullWarranty seller buyer costs shareOfWarranty  = warrantyCostsExogenous seller buyer costs (warrantyCostFunction shareOfWarranty FullWarranty)

----------------------------------
-- 2 Warranty Clauses -- ex ante
----------------------------------
-- | Include probabilistic assessment 
warrantyExAnte seller buyer warrantyCostFunction costs probabilityDistribution= [opengame|

    inputs    :  ;
    feedback  : ;

    :-----:

    inputs    : costs;
    feedback  : ;
    operation : liftStochasticForward probabilityDistribution;
    outputs   : realizedDamage ;
    returns   : ;


    inputs    : realizedDamage;
    feedback  : ;
    operation : warrantyCosts seller buyer warrantyCostFunction ;
    outputs   :  ;
    returns   : ;


    :-----:

    outputs   : ;
    returns   : ;
|]

-- | Specialize to above cost function
warrantyCostsExAnteExogenous seller buyer costs probDamage shareOfWarranty mode = seller buyer warrantyCostFunction' costs probabilityDistribution
 where probabilityDistribution costs = distFromList [(probDamage,costs),(1-probDamage,0)]
       warrantyCostFunction' = warrantyCostFunction shareOfWarranty mode
warrantyCostsExAnteNoWarranty seller buyer costs probDamage shareOfWarranty  = warrantyCostsExAnteExogenous seller buyer costs probDamage shareOfWarranty NoWarranty
warrantyCostsExAnteLimitedWarranty seller buyer costs probDamage shareOfWarranty  = warrantyCostsExAnteExogenous seller buyer costs probDamage shareOfWarranty LimitedWarranty
warrantyCostsExAnteFullWarranty seller buyer costs probDamage shareOfWarranty  = warrantyCostsExAnteExogenous seller buyer costs probDamage shareOfWarranty FullWarranty



