{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Contracts.Subcontracting where

import Engine.Engine
import Preprocessor.Preprocessor


-- Force Majeure clause category

--------------------
-- 2 Building Blocks
--------------------
-- Direct provision of service
-- We include a price to be conditioned on
-- We keep it abstract, same structure repeats itself
-- Quality decision

serviceProviderQuality parameter name actionSpace = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:
    inputs    : ;
    feedback  : ;
    operation : dependentDecision name (const actionSpace) ;
    outputs   : quality ;
    returns   : costQuality parameter quality ;
    :-----:

    outputs   : quality ;
    returns   :  ;
|]


-- Price negotiation
-- NOTE price enters additively
serviceProviderPrice name minPrice maxPrice = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:
    inputs    : ;
    feedback  : ;
    operation : dependentDecision name (const [minPrice, maxPrice]) ;
    outputs   : price ;
    returns   : price ;
    :-----:

    outputs   : price  ;
    returns   :  ;
|]

-- Price negotiation between provider and subcontractor
serviceProviderPriceCondQ name minPrice maxPrice = [opengame|

    inputs    : quality ;
    feedback  : ;

    :-----:
    inputs    : quality ;
    feedback  : ;
    operation : dependentDecision name (const [minPrice, maxPrice]) ;
    outputs   : price ;
    returns   : payoff ;
    :-----:

    outputs   : price  ;
    returns   : payoff ;
|]


--------------------
-- 3 Clause Models
--------------------

-- | No subcontracting
eqNoSubContract minPrice maxPrice actionSpace parameterProvider qualityPref strat = generateIsEq $ evaluate (noSubContract "Provider" "Client" minPrice maxPrice actionSpace parameterProvider qualityPref) strat void

-- | With subcontracting
eqSubContract  minPrice maxPrice actionSpace parameterProvider parameterSubCon qualityPref strat = generateIsEq $ evaluate (subContract "Provider" "Client" "Subcontractor" minPrice maxPrice actionSpace parameterProvider parameterSubCon qualityPref) strat void
