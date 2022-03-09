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


-- Subcontracting clauses

-- TODO Integrate the branching operator; we focus on the single contracts, the non-branching parts
-- TODO Extend wrt quality, other features of the service
---------------
-- 0 Data types
---------------

type Price = Double

data AcceptanceDecision = Accept | Reject
  deriving (Show,Eq,Ord)

data SubcontractingDecision = SubContract | NoSubContract
  deriving (Show,Eq,Ord)

data Quality = High | Low
  deriving (Show,Eq,Ord)

qualityActionSpace = [High, Low]

------------
-- 1 Payoffs
------------

-- Payments for provider if subcontracting takes place
paymentProviderSubContracting accepted price parameterProvider qualityAgreed =
  if accepted
     then (- price - (costQuality parameterProvider qualityAgreed))
     else 0

-- Payments for subcontractor if contract takes place
paymentSubContractor accepted price parameterSubCon qualityAgreed =
  if accepted
     then (price + (costQuality parameterSubCon qualityAgreed))
     else 0

-- costs for producing quality, low fixed at 1
costQuality parameter High = - parameter
costQuality _         Low  = - 1

-- utility for quality, low fixed at 2 (so that benefits from trade)
utilityQuality parameter High = parameter
utilityQuality _         Low  = 2

-- payoff for client
payoffClient qualityPref accepted price qualityAgreed =
  if accepted
     then (utilityQuality qualityPref qualityAgreed) - price
     else 0

-- Auxiliary functions
-- is subcontract accepted?
subContractDecision (price,quality,Accept,Accept) = True
subContractDecision (price,quality,_,_) = False

-- Is contract accepted
contractDecision (price,quality,Accept) = True
contractDecision _                      = False

-- Overwrite if previous contract did not come into existence
contractOverWrite (False, _) =  False
contractOverWrite (_    , x) = x

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


-- Joint offer on quality and price
priceQualityOffer name minPrice maxPrice actionSpace parameter= [opengame|

    inputs    : ;
    feedback  : ;

    :-----:
    inputs    : ;
    feedback  : ;
    operation : serviceProviderPrice name minPrice maxPrice ;
    outputs   : price ;
    returns   :  ;

    inputs    : ;
    feedback  : ;
    operation : serviceProviderQuality parameter name actionSpace ;
    outputs   : quality ;
    returns   : payoff  ;

    :-----:

    outputs   : (price,quality) ;
    returns   : payoff ;
|]




-- Acceptance decision for a given price and quality by the client
acceptanceDec name = [opengame|

    inputs    : (price,quality) ;
    feedback  : ;

    :-----:
    inputs    : (price,quality) ;
    feedback  : ;
    operation : dependentDecision name (const [Accept,Reject]) ;
    outputs   : accDec ;
    returns   : payoff ;
    :-----:

    outputs   : accDec  ;
    returns   : payoff ;
|]


-- Subcontracting negotiation
-- Provider tries to reduce price
subContractNegotiation nameProvider nameSubContractor minPrice maxPrice parameterProvider parameterSubCon= [opengame|

    inputs    : (quality,acceptedOld) ;
    feedback  : ;

    :-----:
    inputs    : quality;
    feedback  : ;
    operation : serviceProviderPriceCondQ nameProvider minPrice maxPrice ;
    outputs   : price ;
    returns   : paymentProviderSubContracting accepted price parameterProvider quality;

    inputs    : (price,quality);
    feedback  : ;
    operation : acceptanceDec nameSubContractor ;
    outputs   : accDec ;
    returns   : paymentSubContractor accepted price parameterSubCon quality;

    inputs    : (price,quality,accDec);
    feedback  : ;
    operation : forwardFunction contractDecision ;
    outputs   : acceptedNew ;
    returns   : ;

    inputs    : (acceptedOld,acceptedNew);
    feedback  : ;
    operation : forwardFunction contractOverWrite ;
    outputs   : accepted ;
    returns   : ;
    :-----:

    outputs   : ;
    returns   : ;
|]


-- include a veto right on the part of the client
subContractNegotiationVeto nameProvider nameSubContractor nameClient minPrice maxPrice parameterProvider parameterSubCon  = [opengame|
    inputs    : (quality,acceptedOld) ;
    feedback  : ;

    :-----:
    inputs    : quality;
    feedback  : ;
    operation : serviceProviderPriceCondQ nameProvider minPrice maxPrice ;
    outputs   : price ;
    returns   : paymentProviderSubContracting accepted price parameterProvider quality;

    inputs    : (price,quality);
    feedback  : ;
    operation : acceptanceDec nameSubContractor ;
    outputs   : accDecSubContractor ;
    returns   : paymentSubContractor accepted price parameterSubCon quality;

    inputs    : (price,quality);
    feedback  : ;
    operation : acceptanceDec nameClient ;
    outputs   : accDecClient ;
    returns   : 0 ;

    inputs    : (price,quality,accDecClient,accDecSubContractor);
    feedback  : ;
    operation : forwardFunction subContractDecision ;
    outputs   : acceptedNew ;
    returns   : ;

    inputs    : (acceptedOld,acceptedNew);
    feedback  : ;
    operation : forwardFunction contractOverWrite ;
    outputs   : accepted ;
    returns   : ;

    :-----:

    outputs   : ;
    returns   : ;
|]

--------------------------------------
-- Contracts
--------------------------------------

-- No subcontracting
noSubContract nameProvider nameClient minPrice maxPrice actionSpace parameterProvider qualityPref = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:
    inputs    : ;
    feedback  : ;
    operation : priceQualityOffer nameProvider minPrice maxPrice actionSpace parameterProvider ;
    outputs   : (price,quality) ;
    returns   : ;

    inputs    : (price,quality);
    feedback  : ;
    operation : acceptanceDec nameClient ;
    outputs   : accDec ;
    returns   : payoffClient qualityPref accepted price quality;

    inputs    : (price,quality,accDec);
    feedback  : ;
    operation : forwardFunction contractDecision ;
    outputs   : accepted ;
    returns   : ;

    :-----:

    outputs   : ;
    returns   : ;
|]


-- With subcontracting single handed decision provider
subContract nameProvider nameClient nameSubContractor minPrice maxPrice actionSpace parameterProvider parameterSubCon qualityPref = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:
    inputs    : ;
    feedback  : ;
    operation : priceQualityOffer nameProvider minPrice maxPrice actionSpace parameterProvider ;
    outputs   : (price,quality) ;
    returns   : ;

    inputs    : (price,quality);
    feedback  : ;
    operation : acceptanceDec nameClient ;
    outputs   : accDec ;
    returns   : payoffClient qualityPref accepted price quality;

    inputs    : (price,quality,accDec);
    feedback  : ;
    operation : forwardFunction contractDecision ;
    outputs   : accepted ;
    returns   : ;

    inputs    : (quality,accepted);
    feedback  : ;
    operation : subContractNegotiation nameProvider nameSubContractor minPrice maxPrice parameterProvider parameterSubCon ;
    outputs   : ;
    returns   : ;

    :-----:

    outputs   : ;
    returns   : ;
|]


-- With subcontracting single handed decision provider
subContractVeto nameProvider nameClient nameSubContractor minPrice maxPrice actionSpace parameterProvider parameterSubCon qualityPref= [opengame|

    inputs    : ;
    feedback  : ;

    :-----:
    inputs    : ;
    feedback  : ;
    operation : priceQualityOffer nameProvider minPrice maxPrice actionSpace parameterProvider ;
    outputs   : (price,quality) ;
    returns   : ;

    inputs    : (price,quality);
    feedback  : ;
    operation : acceptanceDec nameClient ;
    outputs   : accDec ;
    returns   : payoffClient qualityPref accepted price quality;

    inputs    : (price,quality,accDec);
    feedback  : ;
    operation : forwardFunction contractDecision ;
    outputs   : accepted ;
    returns   : ;

    inputs    : (quality,accepted);
    feedback  : ;
    operation : subContractNegotiation nameProvider nameSubContractor minPrice maxPrice  parameterProvider parameterSubCon;
    outputs   : ;
    returns   : ;

    :-----:

    outputs   : ;
    returns   : ;
|]


-----------------------
-- Equilibrium analysis
-----------------------


-- Strategies
stratProviderPrice :: Double -> Kleisli Stochastic () Double
stratProviderPrice x = pureAction x

stratProviderHQuality :: Kleisli Stochastic () Quality
stratProviderHQuality = pureAction High


stratProviderLQuality :: Kleisli Stochastic () Quality
stratProviderLQuality = pureAction Low

stratClient :: Kleisli Stochastic (Double, Quality) AcceptanceDecision
stratClient   =  Kleisli $
  \case
    (price,High) ->
      if price < 4 then playDeterministically Accept else playDeterministically Reject
    (price,Low) ->
      if price < 2 then playDeterministically Accept else playDeterministically Reject

stratSubContractor :: Double -> Kleisli Stochastic (Double, Quality) AcceptanceDecision
stratSubContractor parameter  = Kleisli $
  \case
    (price,High) ->
      if price > parameter then playDeterministically Accept else playDeterministically Reject
    (price,Low) ->
      if price > 1 then playDeterministically Accept else playDeterministically Reject

stratProviderPriceForSubContract :: Double -> Kleisli Stochastic Quality Double
stratProviderPriceForSubContract parameterSubContractor = Kleisli $
  \case
     Low -> playDeterministically 1
     High -> playDeterministically parameterSubContractor



-- | No subcontracting
eqNoSubContract minPrice maxPrice actionSpace parameterProvider qualityPref strat = generateIsEq $ evaluate (noSubContract "Provider" "Client" minPrice maxPrice actionSpace parameterProvider qualityPref) strat void

-- | With subcontracting
eqSubContract  minPrice maxPrice actionSpace parameterProvider parameterSubCon qualityPref strat = generateIsEq $ evaluate (subContract "Provider" "Client" "Subcontractor" minPrice maxPrice actionSpace parameterProvider parameterSubCon qualityPref) strat void



{-
-- The above generates an equilibrium
-- Note if costs are low, the provider chooses high quality
-- eqNoSubContract 1 3 [High,Low] 1 4 (stratProviderPrice 3 ::- stratProviderHQuality ::- stratClient ::- Nil) is an equilibrium

>>>>>>>>>>>Output
----Analytics begin----
 Strategies are in equilibrium
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:
----Analytics end----
<<<<<<<<<<<<<<<<

-- Note, this stops being the case, if the costs for providing quality are too high:
-- eqNoSubContract 1 3 [High,Low] 2 4 (stratProviderPrice 3 ::- stratProviderHQuality ::- stratClient ::- Nil)

>>>>>>>>>>>>Output
----Analytics begin----
 Strategies are in equilibrium
 NEWGAME:

 Strategies are NOT in equilibrium. Consider the following profitable deviations:

Player: Provider
Optimal Move: Low
Current Strategy: fromFreqs [(High,1.0)]
Optimal Payoff: -1.0
Current Payoff: -2.0
Observable State: ()
Unobservable State: "((((),()),3.0),())"
 --other game--
 --No more information--
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:
----Analytics end----
<<<<<<<<<<<<<<<<<

BUT: low quality provision is then an equilibrium (depends, of course, on the parameters chosen)

-- eqNoSubContract 1 3 [High,Low] 2 4 (stratProviderPrice 3 ::- stratProviderLQuality ::- stratClient ::- Nil)

>>>>>>>>>>>>Output
----Analytics begin----
 Strategies are in equilibrium
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:
----Analytics end----
<<<<<<<<<<<<<<<<<


----------------------
-- With subcontracting

-- As before, high quality can be an equilibrium, but here even with a low quality provider (who just uses a high quality subcontractor)
-- eqSubContract 1 3 [High,Low] 1 3 4 (stratProviderPrice 3 ::- stratProviderHQuality ::- stratClient ::- (stratProviderPriceForSubContract 3)  ::- (stratSubContractor 3) ::- Nil)

>>>>>>>>>>>>Output
----Analytics begin----
 Strategies are in equilibrium
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:

 Strategies are in equilibrium
 NEWGAME:
----Analytics end----
<<<<<<<<<<<<<<<<<

-}
